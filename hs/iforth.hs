import Data.Boolean {- Boolean -}
import qualified Text.Read as R {- base -}

import Forth

instance Boolean Integer where
  true = -1
  false = 0
  notB n = if n /= 0 then 0 else -1
  p &&* q = if p /= 0 && q /= 0 then -1 else 0
  p ||* q = if p /= 0 || q /= 0 then -1 else 0

bool_int :: Bool -> Integer
bool_int t = if t then -1 else 0

main :: IO ()
main = do
  let d :: Dict () Integer
      d = concat [core_dict,show_dict,stack_dict,num_dict,int_dict,cmp_dict bool_int]
  run (empty_vm () R.readMaybe) {dict = d}
