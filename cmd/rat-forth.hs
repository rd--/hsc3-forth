import Control.Concurrent {- base -}
import Data.Ratio {- base -}
import System.IO {- base -}
import qualified Text.Read as Read {- base -}

import Safe {- safe -}

import qualified Data.Map as Map {- containers -}

import qualified Language.Forth.Interpreter.Simple as Forth {- hsc3-forth -}

-- * Rational

sep :: Eq a => a -> [a] -> ([a], [a])
sep c s = let (lhs, rhs) = break (== c) s in (lhs, tailDef [] rhs)

bimap1 :: (t -> t1) -> (t, t) -> (t1, t1)
bimap1 f (p, q) = (f p, f q)

parse_int :: String -> Maybe Integer
parse_int = Read.readMaybe

parse_rat :: String -> Maybe Rational
parse_rat s =
  case bimap1 parse_int (sep '/' s) of
    (Just n, Just d) -> Just (n % d)
    _ ->
      case parse_int s of
        Just i -> Just (fromInteger i)
        Nothing -> fmap realToFrac (Read.readMaybe s :: Maybe Double)

rat_pp :: (Show i, Integral i) => Ratio i -> String
rat_pp r =
  let n = numerator r
      d = denominator r
  in if d == 1 then show n else concat [show n, "/", show d]

-- * Primitives

instance (Show i, Integral i) => Forth.Forth_Type (Ratio i) where
  ty_show = rat_pp
  ty_to_int = Just . floor
  ty_from_int = fromIntegral
  ty_from_bool t = if t then -1 else 0

{-
-- | Unary stack operation.
unary_op :: (a -> a) -> Forth w a ()
unary_op f = Forth.pop >>= Forth.push . f
-}

binary_op'' :: (i -> a) -> (a -> i) -> (a -> a -> a) -> Forth.Forth w i ()
binary_op'' f g h = Forth.pop >>= \y -> Forth.pop >>= \x -> Forth.push (g (h (f x) (f y)))

binary_op' :: (Integer -> Integer -> Integer) -> Forth.Forth w Rational ()
binary_op' = binary_op'' floor fromInteger

-- | Binary stack operation.  The first value on the stack is the RHS.
binary_op :: (a -> a -> a) -> Forth.Forth w a ()
binary_op f = Forth.pop >>= \y -> Forth.pop >>= \x -> Forth.push (f x y)

-- | 'binary_op', /rep/ translates the result so it can be placed onto the stack.
comparison_op :: Forth.Forth_Type a => (a -> a -> Bool) -> Forth.Forth w a ()
comparison_op f = binary_op (\x y -> Forth.ty_from_bool (f x y))

-- | Forth word @/mod@.
fw_div_mod :: Forth.Forth w Rational ()
fw_div_mod =
  Forth.pop >>= \p ->
    Forth.pop >>= \q ->
      let (r, s) = floor q `divMod` floor p
      in Forth.push (fromInteger s) >> Forth.push (fromInteger r)

rat_dict :: Forth.Dict w Rational
rat_dict =
  Map.fromList
    [ ("+", binary_op (+))
    , ("*", binary_op (*))
    , ("-", binary_op (-))
    , -- FRACTIONAL
      ("/", binary_op (/))
    , -- INTEGRAL
      ("mod", binary_op' mod)
    , ("div", binary_op' div)
    , ("div-mod", fw_div_mod)
    , -- EQ
      ("=", comparison_op (==))
    , -- ORD
      ("<", comparison_op (<))
    , ("<=", comparison_op (<=))
    , (">", comparison_op (>))
    , (">=", comparison_op (>=))
    ]

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Forth.Dict () Rational
      d = Map.unions [Forth.core_dict, rat_dict]
      vm = (Forth.empty_vm () parse_rat sig) {Forth.dict = d, Forth.input_port = Just stdin}
      init_f = Forth.load_files ["stdlib.fs", "ratlib.fs"]
  putStrLn "RAT-FORTH"
  Forth.repl vm init_f
