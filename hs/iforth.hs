import qualified Data.Map as M {- containers -}
import qualified Text.Read as R {- base -}
import System.IO {- base -}

import Forth

main :: IO ()
main = do
  let d :: Dict () Integer
      d = M.unions [core_dict,show_dict,stack_dict,num_dict,int_dict,cmp_dict]
  repl (empty_vm () R.readMaybe) {dict = d,input_port = Just stdin}

{-
import Data.Boolean {- Boolean -}

instance Boolean Integer where
  true = -1
  false = 0
  notB n = if n /= 0 then 0 else -1
  p &&* q = if p /= 0 && q /= 0 then -1 else 0
  p ||* q = if p /= 0 || q /= 0 then -1 else 0

bool_int :: Bool -> Integer
bool_int t = if t then -1 else 0
-}
