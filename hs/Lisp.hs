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

-- * Environment

type Dict a = [(String,Cell a)]

data Env a = Env {env_frame :: Dict a
                 ,env_parent :: Maybe (Env a)}
             deriving Show

type VM a r = ExceptT String (StateT (Env a) IO) r

maybe_to_err :: MonadError e m => e -> Maybe a -> m a
maybe_to_err msg = maybe (throwError msg) return

env_parent_err' :: Env a -> VM a (Env a)
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

env_lookup_err' :: String -> Env a -> VM a (Cell a)
env_lookup_err' w = maybe_to_err ("ENV-LOOKUP: " ++ w). env_lookup w

env_lookup_err :: String -> Env a -> Cell a
env_lookup_err w = fromMaybe (error ("ENV-LOOKUP: " ++ w)) . env_lookup w

data Cell a = Symbol String | Atom a
            | Nil | Cons (Cell a) (Cell a)
            | Proc (Cell a -> Cell a)
            | Lambda (Env a) String (Cell a)

instance Eq a => Eq (Cell a) where
    p == q =
        case (p,q) of
          (Atom p0,Atom q0) -> p0 == q0
          (Symbol p0,Symbol q0) -> p0 == q0
          (Nil,Nil) -> True
          (Cons p0 p1,Cons q0 q1) -> p0 == q0 && p1 == q1
          _ -> False -- error "EQ"

instance Show a => Show (Cell a) where
    show c =
        case c of
          Atom a -> show ("ATOM",a)
          Symbol s -> show ("SYMBOL",s)
          Nil -> "Nil"
          Cons p q -> concat ["(cons ",show p," ",show q,")"]
          Proc _ -> "PROC"
          Lambda _ nm code -> concat ["(lambda (",nm,") ",show code,")"]

apply' :: (Eq a,Fractional a,Show a) => Cell a -> Cell a -> Maybe (Cell a)
apply' p a =
    case p of
      Proc f -> Just (f a)
      Lambda env nm code ->
          let (_,res) = eval (env_add_binding env nm a) code
          in Just res
      _ -> Nothing

apply :: (Eq a,Fractional a,Show a) => Cell a -> Cell a -> Cell a
apply p = maybe (error ("APPLY: " ++ show p)) id . apply' p

cons :: Cell a
cons = Proc (\lhs -> Proc (\rhs -> Cons lhs rhs))

atom :: Show a => String -> Cell a -> a
atom err c =
    case c of
      Atom a -> a
      _ -> error (unwords ["NOT-ATOM?",err,show c])

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

parse_sexp :: String -> SEXP
parse_sexp = either (error . show) id . S.readExpr

parse_cell' :: (Eq a,Fractional a,Show a) => SEXP -> Cell a
parse_cell' sexp =
    case sexp of
      S.Number n -> Atom (fromIntegral n)
      S.Float n -> Atom (realToFrac n)
      S.Rational n -> Atom (fromRational n)
      S.Atom nm -> Symbol nm
      S.List l -> foldr (\p q -> Cons (parse_cell' p) q)  Nil l
      _ -> error (show "PARSE-CELL")

-- (parse_cell "((a 5) (b 6))")
parse_cell :: (Eq a,Fractional a,Show a) => String -> Cell a
parse_cell = parse_cell' . parse_sexp

-- | Functions are one argument, but allow (+ 1 2) for ((+ 1) 2).
eval_apply :: (Eq a,Fractional a,Show a) => Env a -> Cell a -> Cell a -> Cell a -> (Env a,Cell a)
eval_apply env f p l =
    let (env',p') = eval env p
        r = apply f p'
    in case l of
         Nil -> (env',r)
         Cons e l' -> eval_apply env' r e l'
         _ -> error "EVAL-APPLY"

eval_let :: (Eq a,Fractional a,Show a) => Env a -> Cell a -> Cell a -> (Env a,Cell a)
eval_let env bind code =
    case bind of
      Nil -> eval env code
      Cons (Cons (Symbol nm) (Cons def Nil)) bind'->
          let (env',def') = eval env def
          in eval_let (env_add_binding env' nm def') bind' code
      _ -> error (show ("EVAL-LET",bind,code))

-- > s_list [Symbol "a",Symbol "b",Symbol "c"]
s_list :: [Cell a] -> Cell a
s_list = foldr Cons Nil

rewrite_let_star :: Cell a -> Cell a -> Cell a
rewrite_let_star bind code =
    case bind of
      Cons p Nil -> s_list [Symbol "let",s_list [p],code]
      Cons p bind' -> s_list [Symbol "let",s_list [p],rewrite_let_star bind' code]
      _ -> error "REWRITE-LET*"

eval_lambda :: Show a => Env a -> Cell a -> Cell a -> (Env a,Cell a)
eval_lambda env param code =
    case param of
      Cons (Symbol nm) Nil -> (env,Lambda env nm code)
      Cons nm param' ->
          let code' = s_list [Symbol "lambda",param',code]
          in eval_lambda env (Cons nm Nil) code'
      _ -> error (show ("EVAL-LAMBDA",param,code))

drop_frame :: (Env a,t) -> (Env a,t)
drop_frame (e,c) = (env_parent_err e,c)

eval :: (Eq a,Fractional a,Show a) => Env a -> Cell a -> (Env a,Cell a)
eval env c =
    case c of
      Symbol nm -> (env,env_lookup_err nm env)
      Atom _ -> (env,c)
      Cons (Symbol "define") (Cons (Symbol nm) (Cons def Nil)) ->
          let (env',def') = eval env def
          in (env_add_binding env' nm def',def')
      Cons (Symbol "let") (Cons bind (Cons code Nil)) ->
          let env' = env_add_frame env
          in drop_frame (eval_let env' bind code)
      Cons (Symbol "let*") (Cons bind (Cons code Nil)) ->
          eval env (rewrite_let_star bind code)
      Cons (Symbol "lambda") (Cons param (Cons code Nil)) ->
          eval_lambda env param code
      Cons (Symbol "if") (Cons p (Cons t (Cons f Nil))) ->
          if p == false then eval env f else eval env t
      Cons (Symbol "quote") (Cons code Nil) -> (env,code)
      Cons (Symbol "eval") (Cons code Nil) ->
          let (env',code') = eval env code
          in eval env' code'
      Cons f (Cons p l) ->
          let (env',f') = eval env f
          in eval_apply env' f' p l
      _ -> error (show ("EVAL",env,c))

vm_eval' :: (Eq a,Fractional a,Show a) => Cell a -> VM a (Cell a)
vm_eval' sexp = do
  env <- get
  let (env',res) = eval env sexp
  put env'
  return res

vm_eval :: (Eq a,Fractional a,Show a) => Cell a -> VM a ()
vm_eval sexp = vm_eval' sexp >> return ()

repl' :: (Eq a,Fractional a,Show a) => Env a -> IO ()
repl' vm = do
  str <- getLine
  let sexp = parse_cell str
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
(let ((a 5)) a) ; 5
(let ((a 5) (b 6)) (cons a b)) ; (cons 5 6)
(let ((a 5) (b (+ 2 3))) (* a b)) ; 25
(let* ((a 5) (b (+ a 3))) (* a b)) ; 40
(equal? 5 5) ; #t
(define sq (lambda (n) (* n n)))
(sq 5) ; 25
(define sum-sq (lambda (p q) (+ (sq p) (sq q))))
(sum-sq 7 9) ; 130
(define t 't)
(if t 1 2)
(quote (+ 1 2))
(eval 1) ; 1
(eval (eval 1)) ; 1
(eval (quote (+ 1 2)))
-}
