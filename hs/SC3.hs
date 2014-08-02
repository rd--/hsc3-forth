module SC3 where

import Data.Char {- base -}
import Data.List.Split {- split -}
import Data.Maybe {- base -}
import qualified Text.Read as R {- base -}

import Sound.SC3.ID {- hsc3 -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

-- * List

sep_first :: [t] -> Maybe (t,[t])
sep_first l =
    case l of
      e:l' -> Just (e,l')
      _ -> Nothing

sep_last :: [t] -> Maybe ([t], t)
sep_last =
    let f (e,l) = (reverse l,e)
    in fmap f . sep_first . reverse

-- * UGen

u_halts_mce :: DB.U -> Bool
u_halts_mce = isJust . DB.ugen_mce_input

-- | The halt MCE transform, requires mce is last input.
--
-- > halt_mce_transform [1,2,mce2 3 4] == [1,2,3,4]
halt_mce_transform :: [UGen] -> [UGen]
halt_mce_transform l =
    let (l',e) = fromMaybe (error "HALT MCE TRANSFORM FAILED") (sep_last l)
    in l' ++ mceChannels e

-- | UGen names are given with rate suffixes if oscillators, without if filters.
--
-- > map ugen_sep (words "SinOsc.ar LPF *")
ugen_sep :: String -> Maybe (String,Maybe Rate)
ugen_sep u =
    case splitOn "." u of
      [nm,rt] -> Just (nm,rate_parse (map toUpper rt))
      [nm] -> Just (nm,Nothing)
      _ -> Nothing

ugen_rec :: String -> Maybe DB.U
ugen_rec = DB.uLookup

ugen_io :: DB.U -> (Int,Maybe Int)
ugen_io u =
    (length (DB.ugen_inputs u)
    ,case DB.ugen_nc_input u of
       Nothing -> DB.ugen_outputs u
       Just _ -> Nothing)

-- | SC3 has name overlaps.  '-' is suppressed as a uop (see 'negate')
-- in prefence to the binop ('-'), binops are searched first.
-- Likewise 'Rand' & 'LinRand' are suppressed as a uop if they are
-- given a rate, ie. @Rand.ir@, and allowed as a UGen.
--
-- > map is_uop (words "Abs MIDICPS Neg")
-- > map is_uop (words "- Rand Rand.ir")
is_uop :: String -> Bool
is_uop s = isJust (unaryIndex s)

-- | Max is a UGen (SLUGens) and a binop, to select the UGen supply a rate suffix.
--
-- > map is_binop (words "== > % Trunc Max Max.kr")
is_binop :: String -> Bool
is_binop s = isJust (binaryIndex s)

parse_constant :: String -> Maybe UGen
parse_constant s =
    let d :: Maybe Double
        d = R.readMaybe s
    in fmap constant d

constant_opt :: UGen -> Maybe Double
constant_opt = u_constant . ugen_optimise_ir_rand
