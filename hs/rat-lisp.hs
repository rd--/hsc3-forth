import Data.Ratio {- base -}

import Lisp
import Rational

instance (Show a,Integral a) => Lisp_Ty (Ratio a) where
    ty_show = rat_pp
    ty_from_bool t = if t then 1 else 0

main :: IO ()
main = do
  putStrLn "RAT-LISP"
  env <- env_toplevel :: IO (Env Rational)
  repl env (load_files ["stdlib.lisp","rhs.lisp"])
