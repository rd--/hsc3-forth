module Lisp where

import Control.Concurrent {- mtl -}
import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
import Data.IORef {- base -}
import Data.List {- base -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import System.Directory {- directory -}
import System.IO {- base -}

import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

-- * Types

type Dict a = M.Map String (Cell a)

data Env a = Env {env_frame :: IORef (Dict a)
                 ,env_parent :: Maybe (Env a)}

env_print :: (Show t, Eq t) => Env t -> IO ()
env_print (Env m p) = do
  d <- readIORef m
  print d
  case p of
    Nothing -> return ()
    Just p' -> env_print p'

data Cell a = Symbol String | String String | Boolean Bool | Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Fun (Cell a -> Cell a) | Proc (Cell a -> VM a (Cell a))
            | Lambda (Env a) String (Cell a)
            | Error String

type VM a r = ExceptT String (StateT (Env a) IO) r

class (Eq a,Num a,Fractional a,Show a) => Lisp_Ty a where
instance (Show a,Integral a) => Lisp_Ty (Ratio a) where

-- * Environment

maybe_to_err :: MonadError e m => e -> Maybe a -> m a
maybe_to_err msg = maybe (throwError msg) return

env_parent_err :: Env a -> VM a (Env a)
env_parent_err = maybe_to_err "ENV-PARENT" . env_parent

env_empty :: IO (Env a)
env_empty = do
  d <- newIORef M.empty
  return (Env d Nothing)

env_lookup :: String -> Env a -> IO (Cell a)
env_lookup w (Env m p) = do
  d <- readIORef m
  case M.lookup w d of
    Just r -> return r
    Nothing -> case p of
                 Nothing -> error "ENV-LOOKUP"
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
           Nothing -> error "ENV-SET"

atom :: Cell a -> Maybe a
atom c =
    case c of
      Atom a -> Just a
      _ -> Nothing

instance Eq a => Eq (Cell a) where
    lhs == rhs =
        case (lhs,rhs) of
          (Atom p,Atom q) -> p == q
          (Boolean p,Boolean q) -> p == q
          (String p,String q) -> p == q
          (Symbol p,Symbol q) -> p == q
          (Nil,Nil) -> True
          (Cons p p',Cons q q') -> p == q && p' == q'
          _ -> False -- error "EQ"

list_p :: Eq a => Cell a -> Bool
list_p c =
    case c of
      Cons _ c' -> c' == Nil || list_p c'
      _ -> False

list_pp :: (Eq a,Show a) => Cell a -> String
list_pp c =
    let f l = case l of
                Nil -> []
                Cons e l' -> show e : f l'
                _ -> ["ERROR: NOT LIST"]
    in "(" ++ intercalate "," (f c) ++ ")"

instance (Eq a,Show a) => Show (Cell a) where
    show c =
        case c of
          Atom a -> show a
          Symbol s -> s
          String s -> show s
          Boolean b -> if b then "#t" else "#f"
          Nil -> "'()"
          Cons p q -> if list_p c then list_pp c else concat ["(CONS ",show p," ",show q,")"]
          Fun _ -> "FUN"
          Proc _ -> "PROC"
          Lambda _ nm code -> concat ["(LAMBDA (",nm,") ",show code,")"]
          Error msg -> "ERROR: " ++ msg

lift_uop :: Lisp_Ty a => (a -> a) -> Cell a
lift_uop f = Fun (\c -> maybe (Error "NOT-ATOM?") (Atom . f) (atom c))

lift_binop :: Lisp_Ty a => (a -> a -> a) -> Cell a
lift_binop f =
    let g p q = case (p,q) of
                  (Just p',Just q') -> Atom (f p' q')
                  _ -> Error "NOT-ATOM?"
    in Fun (\lhs -> Fun (\rhs -> g (atom lhs) (atom rhs)))

false :: Cell a
false = Boolean False

true :: Cell a
true = Boolean True

cell_equal :: Eq a => Cell a
cell_equal = Fun (\lhs -> Fun (\rhs -> if lhs == rhs then true else false))

core_dict :: Lisp_Ty a => Dict a
core_dict =
    M.fromList
    [("nil",Nil)
    ,("cons",Fun (\lhs -> Fun (\rhs -> Cons lhs rhs)))
    ,("car",Fun (\c -> case c of {Cons lhs _ -> lhs; _ -> Error ("CAR: " ++ show c)}))
    ,("cdr",Fun (\c -> case c of {Cons _ rhs -> rhs; _ -> Error ("CDR: " ++ show c)}))
    ,("null?",Fun (\c -> case c of {Nil -> true; _ -> false}))
    ,("pair?",Fun (\c -> case c of {Cons _ _ -> true; _ -> false}))
    ,("+",lift_binop (+))
    ,("*",lift_binop (*))
    ,("-",lift_binop (-))
    ,("/",lift_binop (/))
    ,("negate",lift_uop negate)
    ,("recip",lift_uop recip)
    ,("equal?",cell_equal)
    ,("display",Proc (\c -> liftIO (putStr (show c)) >> return c))
    ,("load",l_load)
    ,("eval",l_eval)]

-- > fmap show (env_lookup "add" env_toplevel)
env_toplevel :: Lisp_Ty a => IO (Env a)
env_toplevel = do
  m <- newIORef core_dict
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
      S.Bool b -> return (Boolean b)
      S.List [] -> return Nil
      S.List (e : l) -> do
                e' <- parse_cell' e
                l' <- parse_cell' (S.List l)
                return (Cons e' l')
      _ -> throwError (show "PARSE-CELL")

parse_cell :: Lisp_Ty a => String -> VM a (Cell a)
parse_cell str = parse_sexp str >>= parse_cell'

apply :: Lisp_Ty a => Cell a -> Cell a -> VM a (Cell a)
apply p arg =
    case p of
      Fun f -> return (f arg)
      Proc f -> f arg
      Lambda env nm code -> do
             cur_env <- get -- save current environment
             env' <- liftIO (env_extend [(nm,arg)] env)
             put env' -- put lambda environment
             res <- eval code -- eval code in lambda environment
             put cur_env -- restore environment
             return res
      _ -> throwError ("APPLY: " ++ show p ++ show arg)

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
eval_apply :: Lisp_Ty a => Cell a -> Cell a -> Cell a -> VM a (Cell a)
eval_apply f p l = do
  p' <- eval p
  r <- apply f p'
  case l of
    Nil -> return r
    Cons e l' -> eval_apply r e l'
    _ -> throwError "EVAL-APPLY"

s_list :: [Cell a] -> Cell a
s_list = foldr Cons Nil

rewrite_let :: Eq a => Cell a -> Cell a -> VM a (Cell a)
rewrite_let bind code =
    case bind of
      Cons (Cons nm (Cons def Nil)) bind' -> do
          body <- if bind' == Nil then return code else rewrite_let bind' code
          return (s_list [s_list [Symbol "lambda",s_list [nm],body],def])
      _ -> throwError "REWRITE-LET"

rewrite_let_star :: Cell a -> Cell a -> VM a (Cell a)
rewrite_let_star bind code =
    case bind of
      Cons p Nil -> return (s_list [Symbol "let",s_list [p],code])
      Cons p bind' -> do
                code' <- rewrite_let_star bind' code
                return (s_list [Symbol "let",s_list [p],code'])
      _ -> throwError "REWRITE-LET*"

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

l_load :: Lisp_Ty a => Cell a
l_load = Proc (\c -> load c >> return Nil)

l_eval :: Lisp_Ty a => Cell a
l_eval = Proc (\c -> eval c >>= eval)

eval :: Lisp_Ty a => Cell a -> VM a (Cell a)
eval c =
    case c of
      Symbol nm -> get >>= \env -> liftIO (env_lookup nm env)
      String _ -> return c
      Atom _ -> return c
      Cons (Symbol "set!") (Cons (Symbol nm) (Cons def Nil)) ->
          get >>= \env -> liftIO (env_set env nm def) >> return Nil
      Cons (Symbol "define") (Cons (Symbol nm) (Cons def Nil)) -> do
             env <- get
             def' <- eval def
             liftIO (env_add_binding nm (Symbol "VOID") env >> env_set env nm def')
             return Nil
      Cons (Symbol "if") (Cons p (Cons t (Cons f Nil))) ->
          eval p >>= \p' -> if p' == false then eval f else eval t
      Cons (Symbol "begin") codes -> eval_begin codes
      Cons (Symbol "quote") (Cons code Nil) -> return code
      Cons (Symbol "lambda") (Cons param (Cons code Nil)) ->
          eval_lambda param code
      Cons (Symbol "let") (Cons bind (Cons code Nil)) ->
          eval =<< rewrite_let bind code
      Cons (Symbol "let*") (Cons bind (Cons code Nil)) ->
          eval =<< rewrite_let_star bind code
      Cons f (Cons p l) -> do
          f' <- eval f
          eval_apply f' p l
      _ -> return (Error ("EVAL: " ++ show c))

repl' :: Lisp_Ty a => Env a -> IO ()
repl' env = do
  str <- getLine
  (r,env') <- runStateT (runExceptT (parse_cell str >>= eval)) env
  case r of
    Left msg -> putStrLn ("ERROR: " ++ msg) >> repl' env
    Right res -> putStrLn ("RESULT: " ++ show res) >> repl' env'

{-

(define one 1)
one ; 1
(+ 1) ; FUN
((+ 1) 2) ; 3
(+ 1 2) ; 3
(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let* ((a 5) (b (+ a 3))) (* a b)) ; 40
(equal? 5 5) ; #t

((lambda (n) (* n n)) 3) ; 9

(define sq (lambda (n) (* n n)))
(sq 5) ; 25

(define sum-sq (lambda (p q) (+ (sq p) (sq q))))
(sum-sq 7 9) ; 130

(define t #t)
(if t 1 2)

(quote (+ 1 2)) ; (+ 1 2)
(eval 1) ; 1
(eval (eval 1)) ; 1
(eval (quote (+ 1 2))) ; (+ 1 2)
(1) ; ERROR
(display 1) ; 1
(display (+ 1 2)) ; 3
(begin (display 1) (display 2)) ; 12
(define three (begin (display 1) (display 2) 3)) ; 12 3

"string"
(load "/home/rohan/sw/hsc3-forth/lisp/stdlib.lisp")

(define c (cons 1 2))
(car c) ; 1
(cdr c) ; 2
(pair? c) ; #t
(null? c) ; #f
(null? '()) ; #t

(define l (cons 1 (cons 2 (cons 3 '()))))
(null? l) ; #f
(define length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l))))))
(length l) ; 3

(define (lambda (_) (define undef 1)))
undef

(define a '())
(set! a 5)
a

(define l (cons 1 (cons 2 (cons 3 '()))))
(define length (lambda (l) (if (null? l) 0 (+ 1 (length (cdr l))))))
(length l)


(define square (lambda (n) (* n n)))
((lambda (x y z) (+ x (+ y (square z)))) 1 2 3) ; 12

(define a 5)
(define b (lambda (_) a))
(b '()) ; 5
(set! a 4)
(b '()) ; 4

(define square (lambda (n) (* n n)))
(define f (lambda (x y) ((lambda (a b) (+ (+ (* x (square a)) (* y b)) (* a b))) (+ 1 (* x y)) (- 1 y))))
(f 7 9) ; 28088

-}
