import Control.Concurrent {- base -}
import qualified Data.Map as M {- containers -}
import qualified Text.Read as R {- base -}
import System.IO {- base -}

import Forth

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

-- | 'Num' instance words.
num_dict :: Num n => Dict w n
num_dict = M.fromList
    [("+",binary_op (+))
    ,("*",binary_op (*))
    ,("-",binary_op (-))
    ,("negate",unary_op negate)
    ,("abs",unary_op abs)]

integral_dict :: Integral n => Dict w n
integral_dict = M.fromList
    [("mod",binary_op mod)
    ,("/",binary_op div)
    ,("/mod",fw_div_mod)]

ord_dict :: (Forth_Type a,Ord a) => Dict w a
ord_dict = M.fromList
    [("=",comparison_op (==))
    ,("<",comparison_op (<))
    ,("<=",comparison_op (<=))
    ,(">",comparison_op (>))
    ,(">=",comparison_op (>=))]

main :: IO ()
main = do
  let d :: Dict () Integer
      d = M.unions [core_dict,num_dict,integral_dict,ord_dict]
  putStrLn "IFORTH"
  sig <- newMVar False
  repl (empty_vm () R.readMaybe sig) {dict = d,input_port = Just stdin}

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
