module Lisp where

import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.IORef {- base -}
import qualified Data.Map as M {- containers -}
import System.Directory {- directory -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}

import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

-- * Types

class (Eq a,Ord a,Num a,Fractional a) => Lisp_Ty a where
    ty_show :: a -> String -- ^ String representation of /a/, pretty printer.
    ty_from_bool :: Bool -> a -- ^ Boolean value represented in /a/, by convention @1@ and @0@.

type Dict a = M.Map String (Cell a)

data Env a = Env {env_frame :: IORef (Dict a)
                 ,env_parent :: Maybe (Env a)}

env_print :: Lisp_Ty t => Env t -> IO ()
env_print (Env m p) = do
  d <- readIORef m
  print d
  case p of
    Nothing -> return ()
    Just p' -> env_print p'

data Cell a = Void
            | Symbol String | String String
            | Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Fun (Cell a -> Cell a)
            | Proc (Cell a -> VM a (Cell a))
            | Lambda (Env a) String (Cell a)
            | Macro (Cell a)
            | Error String

type VM a r = ExceptT String (StateT (Env a) IO) r

-- * Environment

maybe_to_err :: MonadError e m => e -> Maybe a -> m a
maybe_to_err msg = maybe (throwError msg) return

env_parent_err :: Env a -> VM a (Env a)
env_parent_err = maybe_to_err "ENV-PARENT" . env_parent

env_empty :: IO (Env a)
env_empty = do
  d <- newIORef M.empty
  return (Env d Nothing)

env_lookup :: String -> Env a -> VM a (Cell a)
env_lookup w (Env m p) = do
  d <- liftIO (readIORef m)
  case M.lookup w d of
    Just r -> return r
    Nothing -> case p of
                 Nothing -> throwError ("ENV-LOOKUP: " ++ w)
                 Just p' -> env_lookup w p'

env_extend :: [(String,Cell a)] -> Env a -> IO (Env a)
env_extend d p = do
  m <- newIORef (M.fromList d)
  return (Env m (Just p))

env_add_binding :: String -> Cell a -> Env a -> IO ()
env_add_binding nm c (Env m _) = modifyIORef m (M.insert nm c)

env_set :: Env a -> String -> Cell a -> IO ()
env_set (Env m p) nm c = do
  d <- readIORef m
  if M.member nm d
    then modifyIORef m (M.insert nm c)
    else case p of
           Just p' -> env_set p' nm c
           Nothing -> error ("ENV-SET: " ++ nm)

atom :: Cell a -> Maybe a
atom c =
    case c of
      Atom a -> Just a
      _ -> Nothing

instance Eq a => Eq (Cell a) where
    lhs == rhs =
        case (lhs,rhs) of
          (Atom p,Atom q) -> p == q
          (String p,String q) -> p == q
          (Symbol p,Symbol q) -> p == q
          (Nil,Nil) -> True
          (Cons p p',Cons q q') -> p == q && p' == q'
          _ -> False -- error "EQ"

is_list :: Eq a => Cell a -> Bool
is_list c =
    case c of
      Cons _ c' -> c' == Nil || is_list c'
      _ -> False

to_list :: Lisp_Ty a => Cell a -> [Cell a]
to_list l =
    case l of
      Nil -> []
      Cons e l' -> e : to_list l'
      _ -> [Error "NOT LIST?"]

list_pp :: Lisp_Ty a => Cell a -> String
list_pp c = "(" ++ unwords (map show (to_list c)) ++ ")"

instance Lisp_Ty a => Show (Cell a) where
    show c =
        case c of
          Atom a -> ty_show a
          Symbol s -> s
          String s -> show s
          Nil -> "'()"
          Cons p q -> if is_list c then list_pp c else concat ["(CONS ",show p," ",show q,")"]
          Fun _ -> "FUN"
          Proc _ -> "PROC"
          Lambda _ nm code -> concat ["(","LAMBDA"," (",nm,") ",show code,")"]
          Macro m -> "MACRO: " ++ show m
          Void -> "VOID"
          Error msg -> "ERROR: " ++ msg

l_false :: Lisp_Ty a => Cell a
l_false = Atom (ty_from_bool False)

l_true :: Lisp_Ty a => Cell a
l_true = Atom (ty_from_bool True)

cell_equal :: Lisp_Ty a => Eq a => Cell a
cell_equal = Fun (\lhs -> Fun (\rhs -> if lhs == rhs then l_true else l_false))

core_dict :: Lisp_Ty a => Dict a
core_dict =
    M.fromList
    [("void",Void)
    ,("#t",l_true)
    ,("#f",l_false)
    ,("void?",Fun (\c -> case c of {Void -> l_true; _ -> l_false}))
    ,("symbol?",Fun (\c -> case c of {Symbol _ -> l_true; _ -> l_false}))
    ,("string?",Fun (\c -> case c of {String _ -> l_true; _ -> l_false}))
    ,("cons",Fun (\lhs -> Fun (\rhs -> Cons lhs rhs)))
    ,("car",Fun (\c -> case c of {Cons lhs _ -> lhs; _ -> Error ("CAR: " ++ show c)}))
    ,("cdr",Fun (\c -> case c of {Cons _ rhs -> rhs; _ -> Error ("CDR: " ++ show c)}))
    ,("null?",Fun (\c -> case c of {Nil -> l_true; _ -> l_false}))
    ,("pair?",Fun (\c -> case c of {Cons _ _ -> l_true; _ -> l_false}))
    ,("list?",Fun (Atom . ty_from_bool . is_list))
    ,("equal?",cell_equal)
    ,("display",Proc (\c -> liftIO (putStr (show c)) >> return c))
    ,("load",Proc (\c -> load c >> return Void))
    ,("eval",Proc (\c -> eval c >>= eval))
    ,("error",Proc (\c -> throwError ("ERROR: " ++ show c)))]

-- > fmap show (env_lookup "add" env_toplevel)
gen_toplevel :: Lisp_Ty a => Dict a -> IO (Env a)
gen_toplevel dict = do
  m <- newIORef dict
  return (Env m Nothing)

type SEXP = S.LispVal

parse_sexps :: String -> VM a [SEXP]
parse_sexps = either (throwError . show) return . S.readExprList

parse_sexp :: String -> VM a SEXP
parse_sexp = either (throwError . show) return . S.readExpr

parse_cell' :: Lisp_Ty a => SEXP -> VM a (Cell a)
parse_cell' sexp =
    case sexp of
      S.Number n -> return (Atom (fromIntegral n))
      S.Float n -> return (Atom (realToFrac n))
      S.Rational n -> return (Atom (fromRational n))
      S.Atom nm -> return (Symbol nm)
      S.String s -> return (String s)
      S.Bool b -> return (Atom (ty_from_bool b))
      S.List [] -> return Nil
      S.List (e : l) -> do
                e' <- parse_cell' e
                l' <- parse_cell' (S.List l)
                return (Cons e' l')
      _ -> throwError (show "PARSE-CELL")

parse_cell :: Lisp_Ty a => String -> VM a (Cell a)
parse_cell str = parse_sexp str >>= parse_cell'

apply_lambda :: Lisp_Ty a => Env a -> String -> Cell a -> Cell a -> VM a (Cell a)
apply_lambda env nm code arg = do
  cur_env <- get -- save current environment
  put =<< liftIO (env_extend [(nm,arg)] env) -- put extended lambda environment
  res <- eval code -- eval code in lambda environment
  put cur_env -- restore environment
  return res

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
apply :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> VM a (Cell a)
apply lhs arg var_arg = do
  let msg = s_list [Symbol "LHS:",lhs,Symbol "RHS:",arg,var_arg]
  r <- case lhs of
         Fun f -> eval arg >>= return . f
         Proc f -> eval arg >>= f
         Lambda env nm code -> eval arg >>= apply_lambda env nm code
         _ -> throwError ("APPLY: " ++ show msg)
  case (lhs,var_arg) of
    (_,Nil) -> return r
    (_,Cons e l') -> apply r e l'
    _ -> throwError ("APPLY: RESULT: " ++ show msg)

s_list :: [Cell a] -> Cell a
s_list = foldr Cons Nil

-- in haskell because to write it in HSC3-LISP without let is opaque
rewrite_let :: Eq a => Cell a -> Cell a -> VM a (Cell a)
rewrite_let bind code =
    case bind of
      Cons (Cons nm (Cons def Nil)) bind' -> do
          body <- if bind' == Nil then return code else rewrite_let bind' code
          return (s_list [s_list [Symbol "lambda",s_list [nm],body],def])
      _ -> throwError "REWRITE-LET"

eval_lambda :: Lisp_Ty a => Cell a -> Cell a -> VM a (Cell a)
eval_lambda param code =
    case param of
      Cons (Symbol nm) Nil -> get >>= \env -> return (Lambda env nm code)
      Cons nm param' ->
          let code' = s_list [Symbol "lambda",param',code]
          in eval_lambda (Cons nm Nil) code'
      _ -> throwError (show ("EVAL-LAMBDA",param,code))

eval_begin :: Lisp_Ty a => Cell a -> VM a (Cell a)
eval_begin l =
    case l of
      Cons e Nil -> eval e
      Cons e l' -> eval e >> eval_begin l'
      _ -> throwError ("BEGIN: " ++ show l)

load :: Lisp_Ty a => Cell a -> VM a ()
load c = do
  case c of
    String nm -> do
               x <- liftIO (doesFileExist nm)
               when (not x) (throwError ("LOAD: FILE MISSING: " ++ nm))
               str <- liftIO (readFile nm)
               sexps <- parse_sexps str
               cells <- mapM parse_cell' sexps
               mapM_ eval cells
    _ -> throwError ("LOAD: " ++ show c)

quote :: Cell a -> Cell a
quote c = s_list [Symbol "quote",c]

eval :: Lisp_Ty a => Cell a -> VM a (Cell a)
eval c =
    -- liftIO (putStrLn ("RUN EVAL: " ++ show c)) >>
    case c of
      Void -> return c
      String _ -> return c
      Atom _ -> return c
      Symbol nm -> get >>= \env -> env_lookup nm env
      Cons (Symbol "set!") (Cons (Symbol nm) (Cons def Nil)) ->
          get >>= \env -> liftIO (env_set env nm def) >> return Void
      Cons (Symbol "define") (Cons (Symbol nm) (Cons def Nil)) -> do
             env <- get
             def' <- eval def
             liftIO (env_add_binding nm (Symbol "VOID") env >> env_set env nm def')
             return Void
      Cons (Symbol "if") (Cons p (Cons t (Cons f Nil))) ->
          eval p >>= \p' -> if p' == l_false then eval f else eval t
      Cons (Symbol "begin") codes -> eval_begin codes
      Cons (Symbol "quote") (Cons code Nil) -> return code
      Cons (Symbol "lambda") (Cons param (Cons code Nil)) -> eval_lambda param code
      Cons (Symbol "macro") (Cons code Nil) -> fmap Macro (eval code)
      Cons (Symbol "let") (Cons bind (Cons code Nil)) ->
          eval =<< rewrite_let bind code
      Cons f (Cons p l) -> do
          -- liftIO (putStrLn ("EVAL: RUN APPLY"))
          f' <- eval f
          case f' of
            Macro f'' -> apply f'' (quote (Cons p l)) Nil >>= eval
            _ -> apply f' p l
      _ -> throwError ("EVAL: " ++ show c)

get_sexp :: String -> Handle -> IO String
get_sexp s h = do
  l <- hGetLine h
  r <- hReady h
  let s' = s ++ l
  if r then get_sexp s' h else return s'

repl' :: Lisp_Ty a => Env a -> IO ()
repl' env = do
  str <- get_sexp "" stdin
  (r,env') <- runStateT (runExceptT (parse_cell str >>= eval)) env
  case r of
    Left msg -> putStrLn ("ERROR: " ++ msg) >> repl' env
    Right res -> putStrLn ("RESULT: " ++ show res) >> repl' env'

repl :: Lisp_Ty a => Env a -> VM a () -> IO ()
repl env initialise = do
  (r,env') <- runStateT (runExceptT initialise) env
  case r of
    Left msg -> error ("REPL: INIT ERROR: " ++ msg)
    Right () -> repl' env'

load_files :: Lisp_Ty a => [String] -> VM a ()
load_files nm = do
  r <- liftIO (lookupEnv "HSC3_LISP_DIR")
  case r of
    Nothing -> throwError "HSC3_LISP_DIR NOT SET"
    Just dir -> mapM_ load (map (String . (dir </>)) nm)

-- * Num

(.:) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(.:) = fmap . fmap

lift_uop :: Lisp_Ty a => (a -> a) -> Cell a
lift_uop f = Fun (\c -> maybe (Error "NOT-ATOM?") (Atom . f) (atom c))

lift_binop :: Lisp_Ty a => (a -> a -> a) -> Cell a
lift_binop f =
    let g p q = case (p,q) of
                  (Just p',Just q') -> Atom (f p' q')
                  _ -> Error "NOT-ATOM?"
    in Fun (\lhs -> Fun (\rhs -> g (atom lhs) (atom rhs)))

num_dict :: Lisp_Ty a => Dict a
num_dict =
    M.fromList
    [("+",lift_binop (+))
    ,("*",lift_binop (*))
    ,("-",lift_binop (-))
    ,("/",lift_binop (/))
    ,("<",lift_binop (ty_from_bool .: (<)))
    ,(">",lift_binop (ty_from_bool .: (>)))
    ,("<=",lift_binop (ty_from_bool .: (<=)))
    ,(">=",lift_binop (ty_from_bool .: (>=)))
    ,("negate",lift_uop negate)
    ,("recip",lift_uop recip)]

float_dict :: (Lisp_Ty a,Floating a) => Dict a
float_dict =
    M.fromList
    [("sin",lift_uop sin)
    ,("cos",lift_uop cos)]