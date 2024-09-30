-- | Forth with unary data type.
module Language.Forth.Interpreter.Simple where

import qualified Control.Concurrent {- base -}
import Control.Monad {- base -}
import qualified Data.Char {- base -}
import qualified Data.List {- base -}
import qualified Data.Maybe {- base -}
import qualified System.Environment {- base -}
import qualified System.Exit {- base -}
import qualified System.IO {- base -}

import qualified System.Directory {- directory -}
import qualified System.FilePath {- filepath -}

import qualified Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}

import qualified Data.Hashable as Hashable {- hashable -}
import qualified Data.Map as Map {- containers -}
import qualified System.Posix.Signals as Signals {- unix -}

-- * Virtual Machine

-- | A dictionary is a map of named instructions ('Forth's).
type Dict w a = Map.Map String (Forth w a ())

-- | Class of values that can constitute a 'Forth'.
class Forth_Type a where
  -- | String representation of /a/, pretty printer.
  ty_show :: a -> String

  -- | Coercion, ie. for loop counters.
  ty_to_int :: a -> Maybe Int

  -- | Coercion
  ty_from_int :: Int -> a

  -- | Boolean value represented in /a/, by convention @-1@ and @0@.
  ty_from_bool :: Bool -> a

ty_to_int' :: Forth_Type a => String -> a -> Int
ty_to_int' msg = Data.Maybe.fromMaybe (error ("Not-integer: " ++ msg)) . ty_to_int

instance Forth_Type Integer where
  ty_show = show
  ty_to_int = Just . fromInteger
  ty_from_int = fromIntegral
  ty_from_bool t = if t then -1 else 0

-- | A data cell, for the data stacks.
data Dc a
  = -- | Plain value
    Dc a
  | -- | String
    Dc_String String
  | -- | Execution token
    Dc_Xt String

instance Forth_Type a => Show (Dc a) where
  show dc =
    case dc of
      Dc a -> ty_show a
      Dc_String str -> "String:" ++ tick_quotes str
      Dc_Xt str -> "Xt:" ++ str

-- | Extract plain value from 'Dc', else error.
dc_plain :: Dc a -> Forth w a a
dc_plain dc =
  case dc of
    Dc a -> return a
    _ -> throw_error "Dc-not-value-cell"

-- | A compilation cell, for the compilation stack.
data Cc w a
  = Cc_Word String
  | Cc_Forth (Forth w a ())

-- | Predicate to see if 'Cc' is a particular 'Cc_Word'.
cc_is_word :: String -> Cc w a -> Bool
cc_is_word w cw =
  case cw of
    Cc_Word w' -> w == w'
    _ -> False

-- | The machine is either interpreting or compiling.
data Vm_Mode = Interpret | Compile deriving (Eq, Show)

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data Vm w a = Vm
  { stack :: [Dc a]
  -- ^ The data stack, /the/ stack.
  , rstack :: [Dc a]
  -- ^ The return stack.
  , cstack :: [Cc w a]
  -- ^ The compilation stack.
  , lstack :: [Int]
  -- ^ The array (list) stack.
  , threads :: Map.Map Int Control.Concurrent.ThreadId
  , dict :: Dict w a
  -- ^ The dictionary.
  , locals :: [Dict w a]
  -- ^ The stack of locals dictionaries.
  , buffer :: String
  -- ^ The current line of input text.
  , mode :: Vm_Mode
  -- ^ Basic state of the machine.
  , world :: w
  -- ^ The world, instance state.
  , literal :: String -> Maybe a
  -- ^ Read function for literal values.
  , dynamic :: Maybe (String -> Forth w a ())
  -- ^ Dynamic post-dictionary lookup.
  , input_port :: Maybe System.IO.Handle
  -- ^ Input port if provided.
  , tracing :: Int
  -- ^ Tracing level, -1 = no tracing, c.f. trace.
  , sigint :: Control.Concurrent.MVar Bool
  -- ^ True if a SIGINT signal (user interrupt) has been received.
  }

instance Forth_Type a => Show (Vm w a) where
  show vm =
    concat
      [ "\n Data stack: "
      , unwords (map show (stack vm))
      , "\n Return stack: "
      , unwords (map show (rstack vm))
      , "\n Compile stack depth: "
      , show (length (cstack vm))
      , "\n List stack: "
      , unwords (map show (lstack vm))
      , "\n Threads: "
      , Data.List.intercalate "," (map show (Map.keys (threads vm)))
      , "\n Dict: "
      , unwords (Map.keys (dict vm))
      , "\n Locals: "
      , Data.List.intercalate "," (map (unwords . Map.keys) (locals vm))
      , "\n Buffer: "
      , buffer vm
      , "\n Mode: "
      , show (mode vm)
      , "\n Dymamic: "
      , maybe "No" (const "Yes") (dynamic vm)
      , "\n Input port: "
      , maybe "No" (const "Yes") (input_port vm)
      , "\n Tracing: "
      , show (tracing vm)
      ]

-- | Signals (exceptions) from 'Vm'.
data Vm_Signal = Vm_Eof | Vm_No_Input | Vm_Error String deriving (Eq, Show)

-- | An instruction, the implementation of a /word/.
type Forth w a r = Vm_M Vm_Signal (Vm w a) IO r

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> Control.Concurrent.MVar Bool -> Vm w a
empty_vm w lit sig =
  Vm
    { stack = []
    , rstack = []
    , cstack = []
    , lstack = []
    , threads = Map.empty
    , buffer = ""
    , mode = Interpret
    , dict = Map.empty
    , locals = []
    , world = w
    , literal = lit
    , dynamic = Nothing
    , input_port = Nothing
    , tracing = -1
    , sigint = sig
    }

-- | Reset 'Vm', on error.
vm_reset :: Vm w a -> Vm w a
vm_reset vm =
  vm
    { stack = []
    , rstack = []
    , cstack = []
    , buffer = ""
    , mode = Interpret
    , locals = []
    }

-- | Type specialised variant of 'get' that checks SigInt handler.
get_vm :: Forth w a (Vm w a)
get_vm = do
  vm <- get
  sig <- liftIO (Control.Concurrent.modifyMVar (sigint vm) (\s -> return (False, s)))
  when sig (throw_error "Vm: SigInt")
  return vm

-- | Function with 'Vm' answering modified Vm and result.
with_vm :: (Vm w a -> (Vm w a, r)) -> Forth w a r
with_vm f = get_vm >>= \vm -> let (vm', r) = f vm in put vm' >> return r

-- | Change the world.
vm_modify_world :: (w -> w) -> Forth w a ()
vm_modify_world f = modify (\vm -> vm {world = f (world vm)})

-- * Error

-- | Tracer, levels are 0 = High, 1 = Medium, 2 = Low
trace :: Int -> String -> Forth w a ()
trace k msg = do
  vm <- get_vm
  when (k <= tracing vm) (write_ln msg)

throw_error :: String -> Forth w a r
throw_error = Control.Monad.Except.throwError . Vm_Error

-- | Reader that raises an /unknown word/ error.
unknown_error :: String -> Forth w a r
unknown_error s = throw_error ("Unknown word: " ++ tick_quotes s)

-- * Stack

-- | .6.1.1200
fw_depth :: Forth_Type a => Forth w a ()
fw_depth = do
  vm <- get_vm
  push (ty_from_int (length (stack vm)))

push' :: Dc a -> Forth w a ()
push' x = modify (\vm -> vm {stack = x : stack vm})

-- | Push value onto 'stack'.
push :: a -> Forth w a ()
push = push' . Dc

pushr' :: Dc a -> Forth w a ()
pushr' x = modify (\vm -> vm {rstack = x : rstack vm})

-- | Push value onto 'rstack'.
pushr :: a -> Forth w a ()
pushr = pushr' . Dc

-- | Push value onto 'cstack'.
pushc :: Cc w a -> Forth w a ()
pushc x = modify (\vm -> vm {cstack = x : cstack vm})

-- | Pop indicated 'Vm' stack.  Requires stack read and write functions.
pop_vm_stack :: String -> (Vm w a -> [r]) -> (Vm w a -> [r] -> Vm w a) -> Forth w a r
pop_vm_stack nm read_stack write_stack = do
  vm <- get_vm
  case read_stack vm of
    [] -> throw_error (nm ++ ": stack underflow")
    x : xs -> put (write_stack vm xs) >> return x

pop' :: Forth w a (Dc a)
pop' = pop_vm_stack "data" stack (\vm s -> vm {stack = s})

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop' >>= dc_plain

-- | 'pop' and error if not 'Int'
pop_int :: Forth_Type a => String -> Forth w a Int
pop_int msg = pop >>= return . ty_to_int' msg

popr' :: Forth w a (Dc a)
popr' = pop_vm_stack "return" rstack (\vm s -> vm {rstack = s})

peekr' :: Forth w a (Dc a)
peekr' = do
  vm <- get_vm
  case rstack vm of
    [] -> throw_error ("peekr': stack underflow")
    x : _ -> return x

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = popr' >>= dc_plain

-- | Remove value from 'cstack'.
popc :: Forth w a (Cc w a)
popc = pop_vm_stack "compile" cstack (\vm s -> vm {cstack = s})

-- | ( id len -- )
pop_string :: String -> Forth w a String
pop_string msg = do
  vm <- get_vm
  case stack vm of
    Dc _ : Dc_String str : s' -> put vm {stack = s'} >> return str
    _ -> throw_error ("not-string?" ++ msg)

popl :: Forth w a Int
popl = pop_vm_stack "list" lstack (\vm s -> vm {lstack = s})

-- | .6.1.2500. Left-bracket. Core
fw_open_bracket :: Forth w a ()
fw_open_bracket = modify (\vm -> vm {lstack = (length (stack vm)) : lstack vm})

-- | .6.1.2540. Right-bracket. Core.
fw_close_bracket :: Forth_Type a => Forth w a r -> Forth w a r
fw_close_bracket w = do
  x <- popl
  vm <- get_vm
  push (ty_from_int (length (stack vm) - x))
  w

-- * Token / Expr

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show, Eq)

-- | Pretty print 'Expr'.
expr_pp :: Forth_Type a => Expr a -> String
expr_pp e =
  case e of
    Literal a -> ty_show a
    Word nm -> nm

-- | Dictionary lookup, word should be lower case.
lookup_word :: String -> Vm w a -> Maybe (Forth w a ())
lookup_word k vm =
  case locals vm of
    [] -> Map.lookup k (dict vm)
    l : _ -> case Map.lookup k l of
      Nothing -> Map.lookup k (dict vm)
      r -> r

-- | Parse a token string to an expression.
parse_token :: String -> Forth w a (Expr a)
parse_token s = do
  vm <- get_vm
  case lookup_word s vm of
    Just _ -> return (Word s)
    Nothing ->
      case literal vm s of
        Just l -> return (Literal l)
        Nothing ->
          case dynamic vm of
            Just _ -> return (Word s) -- if there is a dynamic reader, defer...
            Nothing -> unknown_error s

-- | Read buffer until predicate holds, if /pre/ delete preceding white space.
read_until :: Bool -> (Char -> Bool) -> Forth w a (String, String)
read_until pre cf = do
  vm <- get_vm
  let f = if pre then dropWhile Data.Char.isSpace else id
      r = break_on cf (f (buffer vm))
  trace 2 (show ("read_until", mode vm, fst r, length (snd r)))
  put vm {buffer = snd r}
  return r

scan_until :: (Char -> Bool) -> Forth w a String
scan_until = fmap fst . read_until False

{- | Scan a token from 'buffer', Ans Forth type comments are
discarded.  Although 'buffer' is filled by 'hGetLine' it may
contain newline characters because we may include a file.
-}
scan_token :: Forth w a (Maybe String)
scan_token = do
  r <- read_until True Data.Char.isSpace
  case r of
    ([], []) -> write_ln " Ok" >> return Nothing
    ([], rhs) -> throw_error ("scan_token: Null: " ++ rhs)
    ("\\", _) -> scan_until (== '\n') >> scan_token
    ("(", _) -> scan_until (== ')') >> scan_token
    (e, _) -> return (Just e)

{- | .6.2.2125.
Read line from 'input_port' to 'buffer'.
There are two /exceptions/ thrown here,
'Vm_Eof' if an input port is given but returns Eof,
and 'Vm_No_Input' if there is no input port.
-}
fw_refill :: Forth w a ()
fw_refill = do
  vm <- get_vm
  case input_port vm of
    Nothing -> Control.Monad.Except.throwError Vm_No_Input
    Just h -> do
      eof <- liftIO (System.IO.hIsEOF h)
      when eof (Control.Monad.Except.throwError Vm_Eof)
      trace 2 "refill"
      x <- liftIO (System.IO.hGetLine h)
      put (vm {buffer = x})

-- | If 'scan_token' is 'Nothing', then 'fw_refill' and retry.  Tokens are lower case.
read_token :: Forth w a String
read_token = do
  r <- scan_token
  case r of
    Just str -> return (map Data.Char.toLower str)
    Nothing -> fw_refill >> read_token

-- | 'parse_token' of 'read_token'.
read_expr :: Forth w a (Expr a)
read_expr = parse_token =<< read_token

-- * Interpret

{- | 'lookup_word' in the dictionary, if unknown try 'dynamic', if
dynamic gives a word then add it to the dictionary.
-}
interpret_word :: String -> Forth w a ()
interpret_word w = do
  vm <- get_vm
  case lookup_word w vm of
    Just r -> r
    Nothing ->
      case dynamic vm of
        Just f -> let d_r = f w in put vm {dict = Map.insert w d_r (dict vm)} >> d_r
        Nothing -> throw_error ("Unknown word: " ++ tick_quotes w)

-- | Either 'interpret_word' or 'push' literal.
interpret_expr :: Expr a -> Forth w a ()
interpret_expr e =
  case e of
    Word w -> interpret_word w
    Literal a -> push a

-- | 'interpret_expr' of 'read_expr'.
vm_interpret :: Forth w a ()
vm_interpret = read_expr >>= interpret_expr

-- * Compile

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
vm_compile :: (Eq a, Forth_Type a) => Forth w a ()
vm_compile = do
  expr <- read_expr
  trace 2 ("Compile: " ++ expr_pp expr)
  case expr of
    Word ";" -> fw_semicolon -- .6.1.0450
    Word ":" -> throw_error ": in compile context?" -- .6.1.0460
    Word "do" -> pushc (Cc_Word "do") -- .6.1.1240
    Word "i" -> fw_i -- .6.1.1680
    Word "j" -> fw_j -- .6.1.1730
    Word "loop" -> fw_loop -- .6.1.1800
    Word "if" -> pushc (Cc_Word "if") -- .6.1.1700
    Word "else" -> pushc (Cc_Word "else") -- .6.1.1310
    Word "then" -> fw_then -- .6.1.2270
    Word "{" -> fw_open_brace
    Word "s\"" -> fw_s_quote_compiler -- .6.1.2165
    e -> pushc (Cc_Forth (interpret_expr e))

-- | Get instruction at 'Cc' or raise an error.
cw_instr :: Cc w a -> Forth w a ()
cw_instr cw =
  case cw of
    Cc_Word w -> throw_error ("cw_instr: Word: " ++ w)
    Cc_Forth f -> f

-- | Type specialised 'foldl1' of '>>'.
forth_block :: [Forth w a ()] -> Forth w a ()
forth_block = foldl1 (>>)

-- | Add a 'locals' frame.
begin_locals :: Forth w a ()
begin_locals = modify (\vm -> vm {locals = Map.empty : locals vm})

-- | Remove a 'locals' frame.
end_locals :: Forth w a ()
end_locals = modify (\vm -> vm {locals = tail (locals vm)})

{- | Unwind the 'cstack' to the indicated control word.  The result is
the code block, in sequence.  The control word is also removed from
the cstack.
-}
unwind_cstack_to :: String -> Forth w a [Cc w a]
unwind_cstack_to w = do
  vm <- get_vm
  let (r, c) = break (cc_is_word w) (cstack vm)
  put (vm {cstack = tail c})
  return (reverse r)

-- | Either 'vm_interpret' or 'vm_compile', depending on 'mode'.
vm_execute :: (Eq a, Forth_Type a) => Forth w a ()
vm_execute = do
  vm <- get_vm
  case mode vm of
    Interpret -> vm_interpret
    Compile -> vm_compile

type Vm_M e s m r = Control.Monad.Except.ExceptT e (Control.Monad.State.StateT s m) r

vm_run :: Vm_M e s m r -> s -> m (Either e r, s)
vm_run x = Control.Monad.State.runStateT (Control.Monad.Except.runExceptT x)

vm_execute_buffer :: (Forth_Type a, Eq a) => Vm w a -> IO (Vm w a)
vm_execute_buffer vm = do
  (r, vm') <- vm_run vm_execute vm
  case r of
    Left err -> case err of
      Vm_No_Input -> return vm'
      _ -> error ("vm_execute_buffer: " ++ show err)
    Right () -> vm_execute_buffer vm'

-- * Do Loop

-- | A loop ends when the two elements at the top of the rstack are equal.
loop_end :: Eq a => Forth w a Bool
loop_end = do
  vm <- get_vm
  case rstack vm of
    Dc p : Dc q : _ -> return (p == q)
    _ -> throw_error "loop-end: Illegal rstack (Dc)"

-- | /code/ is the expressions between @do@ and @loop@.
interpret_do_loop :: (Forth_Type a, Eq a) => Forth w a () -> Forth w a ()
interpret_do_loop code = do
  start <- pop
  end <- pop
  pushr end
  pushr start
  let step = do
        code
        i <- popr
        let i' = ty_from_int (ty_to_int' "do-loop: I" i + 1)
        pushr i'
  let loop = do
        r <- loop_end
        if not r then step >> loop else popr >> popr >> return ()
  loop

-- | Compile @loop@ statement, end of do block.
fw_loop :: (Eq a, Forth_Type a) => Forth w a ()
fw_loop = do
  cw <- unwind_cstack_to "do"
  let w = forth_block (map cw_instr cw)
  pushc (Cc_Forth (interpret_do_loop w))

-- * If Else Then

-- | Consult stack and select either /true/ or /false/.
interpret_if :: (Eq a, Forth_Type a) => (Forth w a (), Forth w a ()) -> Forth w a ()
interpret_if (t, f) = pop >>= \x -> if x /= ty_from_bool False then t else f

-- | Compile @then@ statement, end of @if@ block.
fw_then :: (Eq a, Forth_Type a) => Forth w a ()
fw_then = do
  cw <- unwind_cstack_to "if"
  let f = forth_block . map cw_instr
  case break (cc_is_word "else") cw of
    (tb, []) -> pushc (Cc_Forth (interpret_if (f tb, return ())))
    (tb, fb) -> pushc (Cc_Forth (interpret_if (f tb, f (tail fb))))

-- * Locals

-- | Variant on @(local)@, argument not on stack.
fw_local' :: String -> Forth w a ()
fw_local' nm = do
  vm <- get_vm
  case stack vm of
    e : s' ->
      put
        vm
          { stack = s'
          , locals = case locals vm of
              [] -> error "no locals frame"
              l : l' -> Map.insert nm (push' e) l : l'
          }
    _ -> throw_error ("(Local): stack underflow: " ++ nm)

-- | Function over current locals 'Dict'.
at_current_locals :: (Dict w a -> Dict w a) -> Vm w a -> Vm w a
at_current_locals f vm =
  case locals vm of
    l : l' -> vm {locals = f l : l'}
    _ -> error "at_current_locals"

{- | 'locals' is used both during compilation and interpretation.  In
compilation the Rhs is undefined, it is used for name lookup and to
know if an interpreter 'locals' frame must be made.  In
interpretation, if required, it is a secondary dictionary,
consulted first.
-}
fw_open_brace :: Forth_Type a => Forth w a ()
fw_open_brace = do
  let get_names r = do
        w <- read_token
        if w == "}" then return r else get_names (w : r)
  nm <- get_names []
  when (any is_reserved_word nm) (throw_error ("fw_open_brace: reserved word: " ++ unwords nm))
  trace 0 ("Define-locals: " ++ Data.List.intercalate " " nm)
  let locals' = Map.fromList (zip nm (repeat undefined))
  modify (\vm -> at_current_locals (Map.union locals') vm)
  pushc (Cc_Forth (forth_block (map fw_local' nm)))

-- * Compiler

{- | ":".
Enter compile phase.
The word name is pushed onto the /empty/ 'cstack', and a 'locals' frame is added.
-}
fw_colon :: Forth w a ()
fw_colon = do
  nm <- read_token
  trace 0 ("define: " ++ nm)
  when (is_reserved_word nm) (throw_error ("':' reserved name: " ++ nm))
  vm <- get_vm
  when (not (null (cstack vm))) (throw_error ("':' cstack not empty: " ++ nm))
  put
    ( vm
        { mode = Compile
        , cstack = [Cc_Word nm]
        , locals = Map.empty : locals vm
        }
    )

{- | .6.1.0450.  ";".  Semicolon.  Core.
End compile phase.
There is always a compile 'locals' frame to be removed.
-}
fw_semicolon :: Forth w a ()
fw_semicolon = do
  vm <- get_vm
  case reverse (cstack vm) of
    Cc_Word nm : cw ->
      let instr = (map cw_instr cw)
          instr' =
            if Map.null (head (locals vm))
              then instr
              else bracketed (begin_locals, end_locals) instr
          w = forth_block instr'
      in do
          trace 2 ("End definition: " ++ nm)
          when (Map.member nm (dict vm)) (write_sp ("Redefined " ++ nm))
          put
            ( vm
                { cstack = []
                , locals = tail (locals vm)
                , dict = Map.insert nm w (dict vm)
                , mode = Interpret
                }
            )
    _ -> throw_error "cstack"
  return ()

-- * Strings

fw_s_quote_compiler :: Forth_Type a => Forth w a ()
fw_s_quote_compiler = do
  str <- scan_until (== '"')
  trace 2 ("compile: s\": \"" ++ str ++ "\"")
  pushc (Cc_Forth (push_str str))

fw_s_quote_interpet :: Forth_Type a => Forth w a ()
fw_s_quote_interpet = scan_until (== '"') >>= push_str

-- | 6.1.2310
fw_type :: Forth w a ()
fw_type = pop_string "Type" >>= write

-- * Forth words

{- | Store current buffer & input port,
place input string on buffer with no input port,
'vm_execute_buffer',
restore buffer & port.
-}
fw_evaluate' :: (Eq a, Forth_Type a) => String -> Forth w a ()
fw_evaluate' str = do
  vm <- get_vm
  let buf = buffer vm
      ip = input_port vm
  vm' <- liftIO (vm_execute_buffer (vm {buffer = str, input_port = Nothing}))
  put (vm' {buffer = buf, input_port = ip})

-- | Variant on @included@, argument not on stack.
fw_included' :: (Eq a, Forth_Type a) => FilePath -> Forth w a ()
fw_included' nm = do
  trace 0 ("included': " ++ nm)
  x <- liftIO (System.Directory.doesFileExist nm)
  when (not x) (throw_error ("included': file missing: " ++ tick_quotes nm))
  liftIO (readFile nm) >>= fw_evaluate'

fw_included :: (Eq a, Forth_Type a) => Forth w a ()
fw_included = pop_string "included" >>= fw_included'

-- | .6.1.1680.  "i".  Core.  Innermost loop index.
fw_i :: Forth w a ()
fw_i =
  let w = do
        x <- popr
        pushr x
        push x
  in pushc (Cc_Forth w)

-- | .6.1.1730.  "j".  Core.  Next-innermost loop index.
fw_j :: Forth w a ()
fw_j =
  let w = do
        x <- popr
        y <- popr
        z <- popr
        pushr z
        pushr y
        pushr x
        push z
  in pushc (Cc_Forth w)

{- | dup : ( p -- p p ) swap : ( p q -- q p ) drop : ( p -- ) over : (
p q -- p q p ) rot : ( p q r -- q r p ) 2dup : ( p q -- p q p q )
-}
fw_dup, fw_swap, fw_drop, fw_over, fw_rot, fw_2dup :: Forth w a ()
fw_dup = pop' >>= \e -> push' e >> push' e
fw_swap = pop' >>= \p -> pop' >>= \q -> push' p >> push' q
fw_drop = pop' >> return ()
fw_over = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q
fw_rot = pop' >>= \p -> pop' >>= \q -> pop' >>= \r -> push' q >> push' p >> push' r
fw_2dup = pop' >>= \p -> pop' >>= \q -> push' q >> push' p >> push' q >> push' p

-- | .6.2.2030.  ( xu ... x1 x0 u -- xu ... x1 x0 xu )
fw_pick :: Forth_Type a => Forth w a ()
fw_pick = do
  vm <- get_vm
  case stack vm of
    Dc n : s' ->
      let n' = ty_to_int' "pick" n
          e = s' !! n'
      in put vm {stack = e : s'}
    _ -> throw_error "pick"

write, write_ln, write_sp :: String -> Forth w a ()
write = liftIO . putStr
write_ln = write . (++ "\n")
write_sp = write . (++ " ")

-- | .6.1.1320. "emit"  Core
fw_emit :: Forth_Type a => Forth w a ()
fw_emit = write . return . toEnum =<< pop_int "emit"

-- | .6.1.0180. "."  Core
fw_dot :: Forth_Type a => Forth w a ()
fw_dot = write_sp . show =<< pop'

-- | 15.6.1.0220. ".s"  Tools
fw_dot_s :: Forth_Type a => Forth w a ()
fw_dot_s = do
  vm <- get_vm
  let l = map show (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  write (n ++ concatMap (++ " ") l)

-- | .6.1.1750.  "key"  Core
fw_key :: Forth_Type a => Forth w a ()
fw_key = liftIO getChar >>= \c -> push (ty_from_int (fromEnum c))

-- | 15.6.2.0830.  "bye"  Tools
fw_bye :: Forth w a ()
fw_bye = liftIO System.Exit.exitSuccess

push_str :: Forth_Type a => String -> Forth w a ()
push_str str = modify (\vm -> vm {stack = Dc (ty_from_int (length str)) : Dc_String str : stack vm})

fw_vmstat :: Forth_Type a => Forth w a ()
fw_vmstat = get_vm >>= write_ln . show

fw_trace :: Forth_Type a => Forth w a ()
fw_trace = pop >>= \k -> modify (\vm -> vm {tracing = ty_to_int' "trace" k})

fw_fork :: Forth_Type a => Forth w a ()
fw_fork = do
  nm <- read_token
  vm <- get_vm
  case lookup_word nm vm of
    Just fw -> do
      th <- liftIO (Control.Concurrent.forkIO (exec_err vm fw >> return ()))
      let k = Hashable.hash th :: Int
      put
        vm
          { stack = Dc (ty_from_int k) : stack vm
          , threads = Map.insert k th (threads vm)
          }
    Nothing -> throw_error ("Fork: unknown word: " ++ nm)

fw_kill :: Forth_Type a => Forth w a ()
fw_kill = do
  k <- pop_int "Kill: pid?"
  vm <- get_vm
  let threads' = threads vm
  case Map.lookup k threads' of
    Nothing -> throw_error ("Kill: unknown thread: " ++ show k)
    Just th -> liftIO (Control.Concurrent.killThread th) >> put vm {threads = Map.delete k threads'}

fw_kill_all :: Forth w a ()
fw_kill_all = do
  vm <- get_vm
  let th = Map.elems (threads vm)
  liftIO (mapM_ Control.Concurrent.killThread th)
  put vm {threads = Map.empty}

-- | .6.1.0070
fw_tick :: Forth w a ()
fw_tick = do
  tok <- read_token
  push' (Dc_Xt tok)

-- | .6.1.1370. "execute".  Core
fw_execute :: Forth w a ()
fw_execute = do
  c <- pop'
  case c of
    Dc_Xt xt -> interpret_word xt
    _ -> throw_error "Execute: not execution token"

-- * Dictionaries

core_dict :: (Eq a, Forth_Type a) => Dict w a
core_dict =
  let err nm = throw_error (concat [tick_quotes nm, ": compiler word in interpeter context"])
  in Map.fromList
      [ (":", fw_colon) -- .6.1.0450
      , (";", err ";")
      , ("s\"", fw_s_quote_interpet) -- .6.1.2165
      , ("included", fw_included) -- 11.6.1.1718
      , ("type", fw_type) -- .6.1.2310
      , ("do", err "do")
      , ("i", err "i")
      , ("j", err "j")
      , ("loop", err "loop")
      , ("if", err "if")
      , ("else", err "else")
      , ("then", err "then")
      , ("{", err "{")
      , ("}", err "}")
      , ("'", fw_tick) -- .6.1.0070
      , ("execute", fw_execute) -- .6.1.1370
      , ("fork", fw_fork)
      , ("kill", fw_kill)
      , ("killall", fw_kill_all)
      , ("bye", fw_bye) -- 15.6.2.0830
      , -- Stack
        ("depth", fw_depth) -- .6.1.1200
      , ("drop", fw_drop) -- .6.1.1260
      , ("dup", fw_dup) -- .6.1.1290
      , ("over", fw_over) -- .6.1.1990
      , ("pick", fw_pick) -- .6.2.2030
      , ("rot", fw_rot) -- .6.1.2160
      , ("swap", fw_swap) -- .6.1.2260
      , ("2dup", fw_2dup) -- .6.1.0380
      , (">r", pop' >>= pushr') -- .6.1.0580
      , ("r>", popr' >>= push') -- .6.1.2060
      , ("r@", peekr' >>= push') -- 6.1.2070
      , -- Io
        ("emit", fw_emit) -- .6.1.1320
      , (".", fw_dot) -- .6.1.0180
      , (".s", fw_dot_s) -- 15.6.1.0220
      , ("key", fw_key) -- .6.1.1750
      , -- Debug
        ("vmstat", fw_vmstat)
      , ("trace", fw_trace)
      ]

core_words :: [String]
core_words = Map.keys (core_dict :: Dict w Integer)

is_reserved_word :: String -> Bool
is_reserved_word nm = nm `elem` core_words

-- * Operation

exec_err :: Vm w a -> Forth w a () -> IO (Vm w a)
exec_err vm fw = do
  (r, vm') <- vm_run fw vm
  case r of
    Left err -> error ("exec_err: " ++ show err)
    Right () -> return vm'

{- | Read, evaluate, print, loop.
Prints @OK@ at end of line.
Prints error message and runs 'vm_reset' on error.
-}
repl' :: (Eq a, Forth_Type a) => Vm w a -> IO ()
repl' vm = do
  (r, vm') <- vm_run vm_execute vm
  case r of
    Left err -> case err of
      Vm_Eof -> putStrLn "bye" >> liftIO System.Exit.exitSuccess
      Vm_No_Input -> liftIO System.Exit.exitSuccess
      Vm_Error msg -> putStrLn (" error: " ++ msg) >> repl' (vm_reset vm)
    Right () -> repl' vm'

catch_sigint :: Vm w a -> IO ()
catch_sigint vm = do
  let h = Control.Concurrent.modifyMVar_ (sigint vm) (return . const True)
  _ <- Signals.installHandler Signals.sigINT (Signals.Catch h) Nothing
  _ <- Signals.installHandler Signals.sigTERM (Signals.Catch h) Nothing
  return ()

-- | 'repl'' but with 'catch_sigint'.
repl :: (Forth_Type a, Eq a) => Vm w a -> Forth w a () -> IO ()
repl vm init_f = do
  catch_sigint vm
  (_, vm') <- vm_run init_f vm
  repl' vm'

load_files :: (Eq a, Forth_Type a) => [String] -> Forth w a ()
load_files nm = do
  trace 0 ("load-files: " ++ Data.List.intercalate "," nm)
  r <- liftIO (System.Environment.lookupEnv "HSC3_FORTH_DIR")
  case r of
    Nothing -> throw_error "HSC3_FORTH_DIR not set"
    Just dir -> mapM_ fw_included' (map (dir System.FilePath.</>) nm)

-- * List functions

{- | Read until /f/ is 'True', discarding /x/, RHS may be @[]@.

>>> break_on Data.Char.isSpace ""
("","")

>>> break_on (== ')') "comment ) WORD"
("comment "," WORD")

>>> break_on (== '\n') " comment\n\n"
(" comment","\n")
-}
break_on :: (a -> Bool) -> [a] -> ([a], [a])
break_on f l =
  case break f l of
    (lhs, []) -> (lhs, [])
    (lhs, _ : rhs) -> (lhs, rhs)

-- | 'snd' of 'break_on'.
delete_until :: (a -> Bool) -> [a] -> [a]
delete_until f = snd . break_on f

bracketed :: (a, a) -> [a] -> [a]
bracketed (l, r) x = l : x ++ [r]

-- | Bracket with single (tick) quotes.
tick_quotes :: String -> String
tick_quotes = bracketed ('\'', '\'')
