module Forth where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import System.IO {- base -}
import System.Exit {- base -}

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}

-- | A definition is a named instructions ('Forth's).
type Def w a = (String,Forth w a ())

-- | A dictionary is a list of definitions.
type Dict w a = [Def w a]

-- | The machine is either interpreting or compiling.
data VM_Mode = Interpret | Compile deriving (Eq,Show)

-- | Function from a word (text) into an instruction.
type Reader w a = String -> Forth w a ()

-- | Class of values that can constitute a 'Forth'.
class Forth_Type a where
    ty_char :: a -> Char -- ^ Single character representaton of /a/.
    ty_string :: a -> String -- ^ String representation of /a/.
    ty_int :: a -> Int -- ^ Coercion, ie. for loop counters.
    ty_from_int :: Int -> a -- ^ Coercion
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @-1@ and @0@.

instance Forth_Type Integer where
    ty_int = fromInteger
    ty_char = toEnum . fromInteger
    ty_string = show
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

-- | A compilation word, for the compilation stack.
data CW w a = CW_Word String | CW_Forth (Forth w a ())

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a =
    VM {stack :: [a] -- ^ The data stack, /the/ stack.
       ,rstack :: [a] -- ^ The return stack.
       ,cstack :: [CW w a] -- ^ The compilation stack.
       ,dict :: Dict w a -- ^ The dictionary.
       ,buffer :: String -- ^ The current line of input text.
       ,mode :: VM_Mode -- ^ Basic state of the machine.
       ,world :: w -- ^ The world, instance state.
       ,literal :: String -> Maybe a -- ^ Read function for literal values.
       ,dynamic :: Maybe (Reader w a) -- ^ Dynamic post-dictionary lookup.
       ,eol :: Bool -- ^ End of line, runs printer.
       ,input_port :: Handle
       }

-- | An instruction, the implementation of a /word/.
type Forth w a r = ExceptT String (StateT (VM w a) IO) r

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show,Eq)

-- | Tracer.
trace :: String -> Forth w a ()
trace = const (return ()) -- liftIO . putStrLn

with_vm :: MonadState a m => (a -> (a,r)) -> m r
with_vm f = get >>= \vm -> let (vm',r) = f vm in put vm' >> return r

do_with_vm :: MonadState a m => (a -> m a) -> m ()
do_with_vm f = get >>= \vm -> f vm >>= put

-- | Pretty print 'Expr'.
expr_pp :: Forth_Type a => Expr a -> String
expr_pp e =
    case e of
      Literal a -> ty_string a
      Word nm -> nm

-- | Reader that raises an /unknown word/ error.
unknown_error :: Reader w a
unknown_error s = throwError ("unknown word: '" ++ s ++ "'")

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> VM w a
empty_vm w lit =
    VM {stack = []
       ,rstack = []
       ,cstack = []
       ,buffer = ""
       ,mode = Interpret
       ,dict = []
       ,world = w
       ,literal = lit
       ,dynamic = Nothing
       ,eol = False
       ,input_port = stdin}

-- | Push value onto 'stack'.
push :: a -> Forth w a ()
push x = modify (\vm -> vm {stack = x : stack vm})

-- | Push value onto 'rstack'.
pushr :: a -> Forth w a ()
pushr x = modify (\vm -> vm {rstack = x : rstack vm})

-- | Push value onto 'cstack'.
pushc :: CW w a -> Forth w a ()
pushc x = modify (\vm -> vm {cstack = x : cstack vm})

-- | Pop indicated 'VM' stack.
pop_vm_stack :: String -> (VM w a -> [r]) -> (VM w a -> [r] -> VM w a) -> Forth w a r
pop_vm_stack nm f g = do
  vm <- get
  case f vm of
    [] -> throwError (nm ++ ": stack underflow")
    x:xs -> put (g vm xs) >> return x

-- | Remove value from 'stack'.
pop :: Forth w a a
pop = pop_vm_stack "DATA" stack (\vm s -> vm {stack = s})

-- | Remove value from 'rstack'.
popr :: Forth w a a
popr = pop_vm_stack "RETURN" rstack (\vm s -> vm {rstack = s})

-- | Remove value from 'cstack'.
popc :: Forth w a (CW w a)
popc = pop_vm_stack "COMPILE" cstack (\vm s -> vm {cstack = s})

-- | Change the world.
modify_world :: (w -> w) -> Forth w a ()
modify_world f = modify (\vm -> vm {world = f (world vm)})

-- | Delete until /x/ is seen, discard /x/.
delete_until :: Eq a => a -> [a] -> [a]
delete_until x = tail . dropWhile (/= x)

-- | Read a token, ANS Forth type comments are discarded.  At /eof/
-- runs 'exitSuccess'.
read_token :: Forth w a String
read_token = do
  vm <- get
  case buffer vm of
    [] -> do eof <- liftIO isEOF
             if eof then liftIO exitSuccess
                    else do x <- liftIO (hGetLine (input_port vm))
                            put vm {buffer = x}
                            read_token
    str ->
        case break isSpace (dropWhile isSpace str) of
          ("\\",_) -> put vm {buffer = [],eol = True} >> read_token
          ("(",r) -> put vm {buffer = delete_until ')' r} >> read_token
          (e,r) -> put vm {buffer = r,eol = null r} >> if null e then read_token else return e

-- | Dictionary lookup.
lookup_word :: String -> Dict w a -> Maybe (Forth w a ())
lookup_word w d = fmap snd (find ((w ==) . fst) d)

-- | Parse a token string to an expression.
parse_token :: String -> Forth w a (Expr a)
parse_token s = do
  vm <- get
  case lookup_word s (dict vm) of
    Just _  -> return (Word s)
    Nothing ->
        case literal vm s of
          Just l  -> return (Literal l)
          Nothing ->
              case dynamic vm of
                Just _ -> return (Word s) -- if there is an dynamic reader, defer...
                Nothing -> throwError ("unknown word: '" ++ s ++ "'")

-- | 'parse_token' of 'read_token'.
read_expr :: Forth w a (Expr a)
read_expr = parse_token =<< read_token

-- | 'lookup_word' in the dictionary, if unknown try 'dynamic'.
interpret_word :: String -> Forth w a ()
interpret_word w = do
  vm <- get
  case lookup_word w (dict vm) of
    Just r -> r
    Nothing ->
        case dynamic vm of
          Just f -> f w
          Nothing -> throwError ("unknown word: '" ++ w ++ "'")

-- | Either 'interpret_word' or 'push' literal.
interpret_expr :: Expr a -> Forth w a ()
interpret_expr e =
    case e of
      Word w -> interpret_word w
      Literal a -> push a

-- | 'interpret_expr' of 'read_expr'.
interpret :: Forth w a ()
interpret = read_expr >>= interpret_expr

-- | A loop ends when the two elements at the top of the rstack are equal.
loop_end :: Eq a => Forth w a Bool
loop_end = do
  vm <- get
  case rstack vm of
    p:q:_ -> return (p == q)
    _ -> throwError "loop_end: illegal rstack"

-- | /code/ is the expressions between @do@ and @loop@.
interpret_do :: (Forth_Type a,Eq a) => Forth w a () -> Forth w a ()
interpret_do code = do
  start <- pop
  end <- pop
  pushr end
  pushr start
  let step = do
        code
        i <- popr
        let i' = ty_from_int (ty_int i + 1)
        pushr i'
  let loop = do
        r <- loop_end
        if not r then step >> loop else popr >> popr >> return ()
  loop

-- | Forth word @j@.
fw_j :: Forth w a ()
fw_j = do {x <- popr; y <- popr; z <- popr
          ;pushr z; pushr y; pushr x
          ;push z}

-- | Get instruction at 'CW' or raise an error.
cw_instr :: CW w a -> Forth w a ()
cw_instr cw =
    case cw of
      CW_Word w -> throwError ("cw_instr: WORD: " ++ w)
      CW_Forth f -> f

-- | foldl1 of '>>'.
forth_block :: [Forth w a ()] -> Forth w a ()
forth_block = foldl1 (>>)

-- | Compile ';' statement.
end_compilation :: Forth w a ()
end_compilation = do
  vm <- get
  case reverse (cstack vm) of
    CW_Word nm : cw ->
        let w = forth_block (map cw_instr cw)
        in do trace ("END DEFINITION: " ++ nm)
              put (vm {cstack = []
                      ,dict = (nm,w) : dict vm
                      ,mode = Interpret})
    _ -> throwError "CSTACK"
  return ()

-- | Predicate to see if 'CW' is a particular 'CW_Word'.
cw_is_word :: String -> CW w a -> Bool
cw_is_word w cw =
    case cw of
      CW_Word w' -> w == w'
      _ -> False

-- | Unwind the 'cstack' to the indicated control word.  The result is
-- the code block, in sequence.  The control word is also removed from
-- the cstack.
unwind_cstack_to :: String -> Forth w a [CW w a]
unwind_cstack_to w = do
  with_vm (\vm -> let (r,c) = break (cw_is_word w) (cstack vm)
                  in (vm {cstack = tail c},reverse r))

-- | Compile @loop@ statement, end of do block.
end_do :: (Eq a,Forth_Type a) => Forth w a ()
end_do = do
  cw <- unwind_cstack_to "do"
  let w = forth_block (map cw_instr cw)
  pushc (CW_Forth (interpret_do w))

-- | Consult stack and select either /true/ or /false/.
interpret_if :: (Eq a,Forth_Type a) => (Forth w a (),Forth w a ()) -> Forth w a ()
interpret_if (t,f) = pop >>= \x -> if x /= ty_from_bool False then t else f

-- | Compile @then@ statement, end of @if@ block.
end_if :: (Eq a,Forth_Type a) => Forth w a ()
end_if = do
  cw <- unwind_cstack_to "if"
  let f = forth_block . map cw_instr
  case break (cw_is_word "else") cw of
    (tb,[]) -> pushc (CW_Forth (interpret_if (f tb,return ())))
    (tb,fb) -> pushc (CW_Forth (interpret_if (f tb,f (tail fb))))

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
compile :: (Eq a,Forth_Type a) => Forth w a ()
compile = do
  expr <- read_expr
  trace ("COMPILE: " ++ expr_pp expr)
  case expr of
    Word ";" -> end_compilation
    Word "do" -> pushc (CW_Word "do")
    Word "i" -> pushc (CW_Forth (popr >>= \x -> pushr x >> push x))
    Word "j" -> pushc (CW_Forth fw_j)
    Word "loop" -> end_do
    Word "if" -> pushc (CW_Word "if")
    Word "else" -> pushc (CW_Word "else")
    Word "then" -> end_if
    e -> pushc (CW_Forth (interpret_expr e))

-- | Either 'interpret' or 'compile', depending on 'mode'.
execute :: (Eq a,Forth_Type a) => Forth w a ()
execute = do
  vm <- get
  case mode vm of
    Interpret -> interpret
    Compile -> compile

-- | Enter compile phase, ie. ':', the word name is pushed onto the /empty/ 'cstack'.
begin_compile :: Forth w a ()
begin_compile = do
  nm <- read_token
  trace ("DEFINE: " ++ nm)
  let edit vm = do
        when (mode vm == Compile) (throwError "':' in compiler context")
        when (not (null (cstack vm))) (throwError "':' cstack not empty")
        return (vm {mode = Compile, cstack = [CW_Word nm]})
  do_with_vm edit

-- * Primitives

-- | Unary stack operation.
unary_op :: (a -> a) -> Forth w a ()
unary_op f = pop >>= push . f

-- | Binary stack operation.  The first value on the stack is the RHS.
binary_op :: (a -> a -> a) -> Forth w a ()
binary_op f = pop >>= \y -> pop >>= \x -> push (f x y)

-- | 'binary_op', /rep/ translates the result so it can be placed onto the stack.
comparison_op :: Forth_Type a => (a -> a -> Bool) -> Forth w a ()
comparison_op f = binary_op (\x y -> ty_from_bool (f x y))

-- | Put string and then space.
put_str_sp :: String -> IO ()
put_str_sp s = putStr s >> putChar ' '

-- | Forth word @.s@.
fw_show_stack :: Forth_Type a => Forth w a ()
fw_show_stack = do
  vm <- get
  let l = map ty_string (reverse (stack vm))
      n = "<" ++ show (length l) ++ "> "
  liftIO (putStr n >> mapM_ put_str_sp l)

-- * stdlib

-- | Error of compile word in interpeter context.
context_err :: String -> Forth w a ()
context_err nm = do
  let msg = concat ["'",nm,"': compiler word in interpeter context"]
  throwError msg

-- | 'Num' instance words.
num_dict :: Num n => Dict w n
num_dict =
    [("+",binary_op (+))
    ,("*",binary_op (*))
    ,("-",binary_op (-))
    ,("negate",unary_op negate)
    ,("abs",unary_op abs)]

-- | Forth word @/mod@.
fw_div_mod :: Integral a => Forth w a ()
fw_div_mod = pop >>= \p -> pop >>= \q -> let (r,s) = q `divMod` p in push s >> push r

int_dict :: Integral n => Dict w n
int_dict =
    [("mod",binary_op mod)
    ,("/",binary_op div)
    ,("/mod",fw_div_mod)]

frac_dict :: Fractional n => Dict w n
frac_dict =
    [("f/",binary_op (/))]

cmp_dict :: (Forth_Type a,Ord a) => Dict w a
cmp_dict =
    [("=",comparison_op (==))
    ,("<",comparison_op (<))]

stack_dict :: Dict w a
stack_dict =
    [("drop",pop >> return ())
    ,("dup",pop >>= \e -> push e >> push e)
    ,("nip",pop >>= \p -> pop >>= \_ -> push p)
    ,("over",pop >>= \p -> pop >>= \q -> push q >> push p >> push q)
    ,("rot",pop >>= \p -> pop >>= \q -> pop >>= \r -> push q >> push p >> push r)
    ,("swap",pop >>= \p -> pop >>= \q -> push p >> push q)
    ,("tuck",pop >>= \p -> pop >>= \q -> push p >> push q >> push p)
    ,("2dup",pop >>= \p -> pop >>= \q -> push q >> push p >> push q >> push p)]

show_dict :: Forth_Type a => Dict w a
show_dict =
    [("emit",liftIO . putChar . ty_char =<< pop)
    ,(".",liftIO . put_str_sp . ty_string =<< pop)
    ,(".s",fw_show_stack)]

core_dict :: Dict w a
core_dict =
    [(":",begin_compile)
    ,(";",context_err ";")
    ,("do",context_err "do")
    ,("i",context_err "i")
    ,("j",context_err "j")
    ,("loop",context_err "loop")
    ,("if",context_err "if")
    ,("else",context_err "else")
    ,("then",context_err "then")
    ,("bye",liftIO exitSuccess)]

-- * Operation

-- | Read, evaluate, print, loop.  Prints @OK@ at end of line.  Prints
-- errors.  Clears input buffer and resets mode on error.
repl :: (Eq a,Forth_Type a) => VM w a -> IO ()
repl vm = do
  (r,vm') <- runStateT (runExceptT execute) vm
  case r of
    Left err -> putStrLn (" ERROR: " ++ err) >> repl vm {buffer = [],mode = Interpret}
    Right () -> when (eol vm') (putStrLn " OK") >> repl vm'
