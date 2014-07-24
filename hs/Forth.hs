module Forth where

import Data.Char {- base -}
import Data.List {- base -}
import System.IO {- base -}
import System.Exit {- base -}

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.Boolean {- Boolean -}

-- | A definition is a named 'Forth'.
type Def w a = (String,Forth w a ())

-- | A dictionary is a list of definitions.
type Dict w a = [Def w a]

-- | The machine is either interpreting or compiling.
data VM_Mode = Interpret | Compile deriving (Eq,Show)

type Reader w a = String -> Forth w a ()

-- | The machine, /w/ is the type of the world, /a/ is the type of the stack elements.
data VM w a = VM {stack :: [a]
                 ,dict :: Dict w a
                 ,buffer :: String
                 ,mode :: VM_Mode
                 ,world :: w
                 ,literal :: String -> Maybe a
                 ,unknown :: Maybe (Reader w a)}

-- | An instruction, the implementation of a /word/.
type Forth w a r = ExceptT String (StateT (VM w a) IO) r

-- | Expressions are either literals or words.
data Expr a = Literal a | Word String deriving (Show,Eq)

-- | Reader that raises an /unknown word/ error.
unknown_error :: Reader w a
unknown_error s = throwError ("unknown word: '" ++ s ++ "'")

-- | Make an empty (initial) machine.
empty_vm :: w -> (String -> Maybe a) -> VM w a
empty_vm w lit = VM {stack = []
                    ,buffer = ""
                    ,mode = Interpret
                    ,dict = []
                    ,world = w
                    ,literal = lit
                    ,unknown = Nothing}

-- | Push.
push :: a -> Forth w a ()
push x = modify (\vm -> vm {stack = x : stack vm})

-- | Pop.
pop :: Forth w a a
pop = do
  vm <- get
  case stack vm of
    [] -> throwError "pop: stack underflow"
    (x:xs) -> put vm {stack = xs} >> return x

-- | Change the world.
modify_world :: (w -> w) -> Forth w a ()
modify_world f = modify (\vm -> vm {world = f (world vm)})

-- | Delete until /x/ is seen, discard /x/.
delete_until :: Eq a => a -> [a] -> [a]
delete_until x = tail . dropWhile (/= x)

-- | Read a token, comments are discarded.
read_token :: Forth w a String
read_token = do
  vm <- get
  case buffer vm of
    [] -> do eof <- liftIO isEOF
             if eof then liftIO exitSuccess
                    else do x <- liftIO getLine
                            put vm {buffer = x}
                            read_token
    str ->
        case break isSpace (dropWhile isSpace str) of
          ("\\",_) -> put vm {buffer = []} >> read_token
          ("(",r) -> put vm {buffer = delete_until ')' r} >> read_token
          (e,r) -> put vm {buffer = r} >> if null e then read_token else return e

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
          Nothing -> case unknown vm of
                       Just _ -> return (Word s) -- if there is an unknown reader, defer...
                       Nothing -> throwError ("unknown word: '" ++ s ++ "'")

-- | 'parse_token' of 'read_token'.
read_expr :: Forth w a (Expr a)
read_expr = parse_token =<< read_token

interpret_word :: String -> Forth w a ()
interpret_word w = do
  vm <- get
  case lookup_word w (dict vm) of
    Just r -> r
    Nothing -> case unknown vm of
                 Just f -> f w
                 Nothing -> throwError ("unknown word: '" ++ w ++ "'")

interpret_expr :: Expr a -> Forth w a ()
interpret_expr e =
    case e of
      Word w -> interpret_word w
      Literal a -> push a

interpret :: Forth w a ()
interpret = read_expr >>= interpret_expr

interpret_if :: (Eq a,Boolean a) => Forth w a () -> Forth w a () -> Forth w a ()
interpret_if tb fb = pop >>= \x -> if x /= false then tb else fb

compile :: (Eq a,Boolean a) => Forth w a ()
compile = do
  let accumulate a = do
            expr <- read_expr
            case expr of
              Word ";" -> return a
              Word "if" -> compile_if >>= \i -> accumulate (a >> i)
              e -> accumulate (a >> interpret_expr e)
  name <- read_token
  action <- accumulate (return ())
  modify (\vm -> vm {dict = (name,action) : dict vm
                    ,mode = Interpret})

apply_if :: (a,a) -> (a -> a) -> Bool -> (a,a)
apply_if (x,y) f b = if b then (f x,y) else (x,f y)

compile_if :: (Eq a,Boolean a) => Forth w a (Forth w a ())
compile_if =
    let accumulate a q = do
          expr <- read_expr
          case expr of
            Word "if" -> compile_if >>= \i -> accumulate (apply_if a (>> i) q) q
            Word "then" -> return (uncurry interpret_if a)
            Word "else" -> accumulate a False
            e -> accumulate (apply_if a (>> interpret_expr e) q) q
    in accumulate (return (),return ()) True

execute :: (Eq a,Boolean a) => Forth w a ()
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

unary_op :: (a -> a) -> Forth w a ()
unary_op f = pop >>= push . f

binary_op :: (a -> a -> a) -> Forth w a ()
binary_op f = pop >>= \x -> pop >>= \y -> push (f y x)

comparison_op :: (Bool -> a) -> (a -> a -> Bool) -> Forth w a ()
comparison_op rep f = binary_op (\x y -> rep (f x y))

-- * stdlib

-- | Compile word in interpeter context.
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

int_dict :: Integral n => Dict w n
int_dict =
    [("mod",binary_op mod)
    ,("/",binary_op div)
    ,("/mod",pop >>= \p -> pop >>= \q -> let (r,s) = q `divMod` p in push s >> push r)]

frac_dict :: Fractional n => Dict w n
frac_dict =
    [("f/",binary_op (/))]

cmp_dict :: Ord a => (Bool -> a) -> Dict w a
cmp_dict rep =
    [("=",comparison_op rep (==))
    ,("<",comparison_op rep (<))]

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

show_dict :: Show a => Dict w a
show_dict =
    [(".",liftIO . print =<< pop)
    ,(".s",liftIO . print . reverse . stack =<< get)]

core_dict :: Dict w a
core_dict =
    [("bye",liftIO exitSuccess)
    ,("if",context_err "if")
    ,("else",context_err "else")
    ,("then",context_err "then")
    ,(";",context_err ";")
    ,(":",begin_compile)]

-- * Operation

run :: (Eq a,Boolean a) => VM w a -> IO ()
run vm = do
  (result,newState) <- runStateT (runExceptT execute) vm
  case result of
    Left err  -> putStrLn ("error: " ++ err)
    _ -> return ()
  run newState
