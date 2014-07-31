import Control.Monad.State {- mtl -}
import Control.Monad.Except {- mtl -}
--import Data.Char {- base -}
import Data.Maybe {- base -}
--import System.Exit {- base -}
--import System.IO {- base -}

--import qualified Language.Scheme.Core as S {- husk-scheme -}
import qualified Language.Scheme.Parser as S {- husk-scheme -}
import qualified Language.Scheme.Types as S {- husk-scheme -}

--import List
--import Rational

type Dict a = [(String,Cell a)]

data Env a = Env {env_frame :: Dict a
                 ,env_parent :: Maybe (Env a)}
             deriving Show

maybe_to_err :: MonadError e m => e -> Maybe a -> m a
maybe_to_err msg = maybe (throwError msg) return

env_parent_err' :: Env a -> Lisp a (Env a)
env_parent_err' = maybe_to_err "ENV-PARENT" . env_parent

env_parent_err :: Env a -> Env a
env_parent_err = fromMaybe (error "ENV-PARENT") . env_parent

env_empty :: Env a
env_empty = Env [] Nothing

env_add_frame :: Env a -> Env a
env_add_frame e = Env [] (Just e)

env_add_binding :: Env a -> String -> Cell a -> Env a
env_add_binding (Env dict p) nm c = Env ((nm,c) : dict) p

env_lookup :: String -> Env a -> Maybe (Cell a)
env_lookup w (Env m p) =
    case lookup w m of
      Nothing -> maybe Nothing (env_lookup w) p
      Just r -> Just r

env_lookup_err' :: String -> Env a -> Lisp a (Cell a)
env_lookup_err' w = maybe_to_err "ENV-LOOKUP" . env_lookup w

env_lookup_err :: String -> Env a -> Cell a
env_lookup_err w = fromMaybe (error "ENV-LOOKUP") . env_lookup w

data Cell a = Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Proc (Cell a -> Cell a)
            | Lambda (Env a) String SEXP

instance Eq a => Eq (Cell a) where
    p == q =
        case (p,q) of
          (Atom p0,Atom q0) -> p0 == q0
          (Nil,Nil) -> True
          (Cons p0 p1,Cons q0 q1) -> p0 == q0 && p1 == q1
          _ -> undefined

instance Show a => Show (Cell a) where
    show c =
        case c of
          Atom a -> show a
          Nil -> "NIL"
          Cons p q -> unwords ["CONS",show p,show q]
          Proc _ -> "PROC"
          Lambda env nm code -> unwords ["LAMBDA",show env,nm,show code]

apply' :: (Fractional a,Show a) => Cell a -> Cell a -> Maybe (Cell a)
apply' p a =
    case p of
      Proc f -> Just (f a)
      Lambda env nm code ->
          let (_,res) = eval (env_add_binding env nm a) code
          in Just res
      _ -> Nothing

apply :: (Fractional a,Show a) => Cell a -> Cell a -> Cell a
apply p = maybe (error ("APPLY: " ++ show p)) id . apply' p

cons :: Cell a
cons = Proc (\lhs -> Proc (\rhs -> Cons lhs rhs))

atom :: Show a => String -> Cell a -> a
atom err c =
    case c of
      Atom a -> a
      _ -> error (unwords ["NOT ATOM",err,show c])

lift_uop :: Show a => (a -> a) -> Cell a
lift_uop f = Proc (\c -> Atom (f (atom "UOP" c)))

lift_binop :: Show a => (a -> a -> a) -> Cell a
lift_binop f = Proc (\lhs -> Proc (\rhs -> Atom (f (atom "LHS" lhs) (atom "RHS" rhs))))

false :: Cell a
false = Nil

true :: Cell a
true = Cons Nil Nil

cell_equal :: Eq a => Cell a
cell_equal = Proc (\lhs -> Proc (\rhs -> if lhs == rhs then true else false))

core_dict :: (Eq a,Num a,Fractional a,Show a) => Dict a
core_dict =
    [("nil",Nil)
    ,("cons",cons)
    ,("+",lift_binop (+))
    ,("*",lift_binop (*))
    ,("-",lift_binop (-))
    ,("/",lift_binop (/))
    ,("negate",lift_uop negate)
    ,("recip",lift_uop recip)
    ,("equal?",cell_equal)]

-- > fmap show (env_lookup "add" env_toplevel)
env_toplevel :: (Eq a,Num a,Fractional a,Show a) => Env a
env_toplevel = Env core_dict Nothing

type SEXP = S.LispVal

-- > parse_sexp "(define pi 3.141592653589793)"
parse_sexp :: String -> S.LispVal
parse_sexp = either (error . show) id . S.readExpr

type Lisp a r = ExceptT String (StateT (Env a) IO) r

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
eval_apply :: (Show a,Fractional a) => Env a -> Cell a -> SEXP -> [SEXP] -> (Env a,Cell a)
eval_apply env f p l =
    let (env',p') = eval env p
        r = apply f p'
    in case l of
         [] -> (env',r)
         e : l' -> eval_apply env' r e l'

eval_let :: (Show a,Fractional a) => Env a -> [SEXP] -> SEXP -> (Env a,Cell a)
eval_let env bind code =
    case bind of
      [] -> eval env code
      S.List [S.Atom nm,def] : bind' ->
          let (env',def') = eval env def
          in eval_let (env_add_binding env' nm def') bind' code
      _ -> error "EVAL-LET"

rewrite_let_star :: [SEXP] -> SEXP -> SEXP
rewrite_let_star bind code =
    case bind of
      [] -> error "EVAL-LET"
      [p] -> S.List [S.Atom "let",S.List [p],code]
      p:bind' -> S.List [S.Atom "let",S.List [p],rewrite_let_star bind' code]

eval_lambda :: Env a -> [SEXP] -> SEXP -> (Env a,Cell a)
eval_lambda env param code =
    case param of
      [S.Atom nm] -> (env,Lambda env nm code)
      nm : param' ->
          let code' = S.List [S.Atom "lambda",S.List param',code]
          in eval_lambda env [nm] code'
      _ -> error "EVAL-LAMBDA"

drop_frame :: (Env a,t) -> (Env a,t)
drop_frame (e,c) = (env_parent_err e,c)

eval :: (Show a,Fractional a) => Env a -> SEXP -> (Env a,Cell a)
eval env sexp =
    case sexp of
      S.Number n -> (env,Atom (fromIntegral n))
      S.Float n -> (env,Atom (realToFrac n))
      S.Rational n -> (env,Atom (fromRational n))
      S.Atom nm -> (env,env_lookup_err nm env)
      S.List [S.Atom "define",S.Atom nm,def] ->
          let (env',def') = eval env def
          in (env_add_binding env' nm def',def')
      S.List [S.Atom "let",S.List bind,code] ->
          let env' = env_add_frame env
          in drop_frame (eval_let env' bind code)
      S.List [S.Atom "let*",S.List bind,code] ->
          eval env (rewrite_let_star bind code)
      S.List [S.Atom "lambda",S.List param,code] ->
          eval_lambda env param code
      S.List (f : p : l) ->
          let (env',f') = eval env f
          in eval_apply env' f' p l
      _ -> error (show (env,sexp))

vm_eval' :: (Show a,Fractional a) => SEXP -> Lisp a (Cell a)
vm_eval' sexp = do
  env <- get
  let (env',res) = eval env sexp
  put env'
  return res

vm_eval :: (Show a,Fractional a) => SEXP -> Lisp a ()
vm_eval sexp = vm_eval' sexp >> return ()

repl' :: (Fractional a,Show a) => Env a -> IO ()
repl' vm = do
  str <- getLine
  let sexp = parse_sexp str
  (r,vm') <- runStateT (runExceptT (vm_eval' sexp)) vm
  case r of
    Left err -> case err of
                  msg -> putStrLn ("ERROR: " ++ msg) >> repl' vm
    Right res -> putStrLn ("RESULT: " ++ show res) >> repl' vm'

main :: IO ()
main = do
  putStrLn "RAT-LISP"
  repl' (env_toplevel :: Env Rational)

{-
(define one 1)
one ; 1
(+ 1) ; PROC
((+ 1) 2) ; 3
(+ 1 2) ; 3
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let* ((a 5) (b (+ a 3))) (* a b)) ; 40
(equal? 5 5) ; #t
(define sq (lambda (n) (* n n)))
(sq 5) ; 25
(define sum-sq (lambda (p q) (+ (sq p) (sq q))))
(sum-sq 7 9) ; 130
-}
