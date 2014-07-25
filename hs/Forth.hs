module Forth where

import Control.Monad {- base -}
import Data.Char {- base -}
import Data.List {- base -}
import System.IO {- base -}
import System.Exit {- base -}

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
--import Data.Boolean {- Boolean -}

-- | A definition is a named 'Forth'.
type Def w a = (String,Forth w a ())

-- | A dictionary is a list of definitions.
type Dict w a = [Def w a]

-- | The machine is either interpreting or compiling.
data VM_Mode = Interpret | Compile deriving (Eq,Show)

-- | Function from a word (text) into an instruction.
type Reader w a = String -> Forth w a ()

-- | Class of values that can be elements of 'Forth'.
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

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a =
    VM {stack :: [a]
       ,rstack :: [a]
       ,dict :: Dict w a
       ,buffer :: String
       ,mode :: VM_Mode
       ,world :: w
       ,literal :: String -> Maybe a -- ^ Read function for literal values.
       ,dynamic :: Maybe (Reader w a) -- ^ Dynamic post-dictionary lookup.
       ,eol :: Bool
       ,input_port :: Handle
       }

-- | An instruction, the implementation of a /word/.
type Forth w a r = ExceptT String (StateT (VM w a) IO) r

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show,Eq)

-- | Reader that raises an /unknown word/ error.
unknown_error :: Reader w a
unknown_error s = throwError ("unknown word: '" ++ s ++ "'")

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> VM w a
empty_vm w lit =
    VM {stack = []
       ,rstack = []
       ,buffer = ""
       ,mode = Interpret
       ,dict = []
       ,world = w
       ,literal = lit
       ,dynamic = Nothing
       ,eol = False
       ,input_port = stdin}

-- | Push value onto stack.
push :: a -> Forth w a ()
push x = modify (\vm -> vm {stack = x : stack vm})

-- | Push value onto rstack.
pushr :: a -> Forth w a ()
pushr x = modify (\vm -> vm {rstack = x : rstack vm})

sep_stack :: (VM w a -> [a]) -> Forth w a (VM w a,a,[a])
sep_stack f = do
  vm <- get
  case f vm of
    [] -> throwError "stack underflow"
    x:xs -> return (vm,x,xs)

-- | Remove value from stack.
pop :: Forth w a a
pop = sep_stack stack >>= \(vm,x,xs) -> put vm {stack = xs} >> return x

-- | Remove value from stack.
popr :: Forth w a a
popr = sep_stack rstack >>= \(vm,x,xs) -> put vm {rstack = xs} >> return x

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

-- | Apply /f/ at either /true/ or /false/ given 'Bool'.
apply_if :: (a,a) -> (a -> a) -> Bool -> (a,a)
apply_if (x,y) f b = if b then (f x,y) else (x,f y)

-- | Consult stack and select either /true/ or /false/.
interpret_if :: (Eq a,Forth_Type a) => (Forth w a (),Forth w a ()) -> Forth w a ()
interpret_if (t,f) = pop >>= \x -> if x /= ty_from_bool False then t else f

-- | /if/ in compile context.
compile_if :: (Eq a,Forth_Type a) => Forth w a (Forth w a ())
compile_if =
    let accumulate a q = do
          expr <- read_expr
          case expr of
            Word "if" -> compile_if >>= \i -> accumulate (apply_if a (>> i) q) q
            Word "then" -> return (interpret_if a)
            Word "else" -> accumulate a False
            e -> accumulate (apply_if a (>> interpret_expr e) q) q
    in accumulate (return (),return ()) True

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

-- | /do/ in compile context.
compile_do :: (Eq a,Forth_Type a) => Forth w a (Forth w a ())
compile_do =
    let accum a = do
          expr <- read_expr
          case expr of
            Word "do" -> compile_do >>= \r -> accum (a >> r)
            Word "loop" -> return a
            Word "i" -> accum (a >> popr >>= \x -> pushr x >> push x)
            Word "j" -> accum (a >> fw_j)
            e -> accum (a >> interpret_expr e)
    in do code <- accum (return ())
          return (interpret_do code)

-- | Define word and add to dictionary.  The only control structures are /if/ and /do/.
compile :: (Eq a,Forth_Type a) => Forth w a ()
compile = do
  let accumulate a = do
            expr <- read_expr
            case expr of
              Word ";" -> return a
              Word "do" -> compile_do >>= \i -> accumulate (a >> i)
              Word "if" -> compile_if >>= \i -> accumulate (a >> i)
              e -> accumulate (a >> interpret_expr e)
  name <- read_token
  action <- accumulate (return ())
  modify (\vm -> vm {dict = (name,action) : dict vm
                    ,mode = Interpret})

-- | Either 'interpret' or 'compile', depending on 'mode'.
execute :: (Eq a,Forth_Type a) => Forth w a ()
execute = do
  vm <- get
  case mode vm of
    Interpret -> interpret
    Compile -> compile

-- | Enter compile phase, ie. ':'.
begin_compile :: Forth w a ()
begin_compile = do
  vm <- get
  case mode vm of
    Interpret -> put vm {mode = Compile}
    _ -> throwError "begin_compile: ':' in compiler context"

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

-- | Forth word @.s@.
fw_show_stack :: Forth_Type a => Forth w a ()
fw_show_stack = do
  vm <- get
  let l = map ty_string (reverse (stack vm))
      n = " <" ++ show (length l) ++ ">"
      l' = " " : intersperse " " l
  liftIO (putStr n >> mapM_ putStr l')

-- * stdlib

-- | Error of compile word in interpeter context.
context_err :: String -> Forth w a ()
context_err nm = do
  let msg = concat ["'",nm,"': compiler word in interpeter context"]
  throwError msg

num_dict :: Num n => Dict w n
num_dict =
    [("+",binary_op (+))
    ,("-",binary_op (-))
    ,("*",binary_op (*))
    ,("negate",unary_op negate)]

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
    ,(".",liftIO . putStr . (" " ++ ) . ty_string =<< pop)
    ,(".s",fw_show_stack)]

core_dict :: Dict w a
core_dict =
    [("bye",liftIO exitSuccess)
    ,("do",context_err "do")
    ,("loop",context_err "loop")
    ,("i",context_err "i")
    ,("j",context_err "j")
    ,("if",context_err "if")
    ,("else",context_err "else")
    ,("then",context_err "then")
    ,(";",context_err ";")
    ,(":",begin_compile)]

-- * Operation

-- | Read, evaluate, print, loop.  There is no PRINT unless there is an error.
repl :: (Eq a,Forth_Type a) => VM w a -> IO ()
repl vm = do
  (r,vm') <- runStateT (runExceptT execute) vm
  case r of
    Left err -> putStrLn (" ERROR: " ++ err)
    Right () -> when (eol vm') (putStrLn " OK")
  repl vm'
