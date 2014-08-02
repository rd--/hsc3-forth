import qualified Data.Map as M {- containers -}
import Data.Ratio {- base -}

import Lisp
import Rational

instance (Show a,Integral a) => Lisp_Ty (Ratio a) where
    ty_show = rat_pp
    ty_from_bool t = if t then 1 else 0

main :: IO ()
main = do
  putStrLn "RAT-LISP"
  env <- gen_toplevel (M.union core_dict num_dict) :: IO (Env Rational)
  repl env (load_files ["stdlib.lisp","rhs.lisp"])
