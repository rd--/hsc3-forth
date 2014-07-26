import Control.Monad {- base -}
import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Char {- base -}
import Data.List {- base -}
import Data.List.Split {- split -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Data.Ratio {- base -}
import qualified Text.Read as R {- base -}
import System.Environment {- base -}
import System.FilePath {- filepath -}
import System.IO {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3.ID {- hsc3 -}
import Sound.SC3.UGen.MCE {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import Forth

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

-- * Forth

type U_Forth r = Forth Int UGen r
type U_Reader = String -> U_Forth ()

-- | 'replicateM' of 'pop'.
pop_n :: Int -> Forth w a [a]
pop_n n = replicateM n pop

-- | 'mapM_' of 'push'.
push_l :: [a] -> Forth w a ()
push_l = mapM_ push

-- | 'u_constant' of 'pop'.
pop_int :: Forth w UGen Int
pop_int = fmap (floor . u_constant) pop

pop_double :: Forth w UGen Double
pop_double = fmap (realToFrac . u_constant) pop

-- | Get counter and store increment.
incr_id :: Forth Int a Int
incr_id = do
  vm <- get
  let w = world vm
  put vm {world = w + 1}
  return w

-- | Assert that the stack is empty.
assert_empty :: Show a => Forth w a ()
assert_empty = do
  vm <- get
  case stack vm of
    [] -> return ()
    l -> throwError (show ("assert_empty",l))

-- * UGen

-- | Requires mce is last input.  FAKED...
--
-- > map u_halts_mce (words "Drand EnvGen Out")
u_halts_mce :: String -> Bool
u_halts_mce u =
    case DB.uLookup u of
      Just r -> isJust (DB.ugen_mce_input r) || u `elem` ["EnvGen"]
      _ -> False

-- > halt_mce_transform [1,2,mce2 3 4] == [1,2,3,4]
halt_mce_transform :: [UGen] -> [UGen]
halt_mce_transform l =
    let (l',e) = fromMaybe (error "halt_mce_transform") (sep_last l)
    in l' ++ mceChannels e

-- > is_nondet "LFNoise0"
is_nondet :: String -> Bool
is_nondet = flip elem ["BrownNoise","CoinGate","Drand","Dseq","Dust","LFNoise0","LFNoise1","Rand","TExpRand","WhiteNoise"]

-- > ugen_sep "SinOsc.ar" == ("SinOsc",Just AR)
-- > ugen_sep "LPF" == ("LPF",Nothing)
ugen_sep :: String -> (String,Maybe Rate)
ugen_sep u =
    case splitOn "." u of
      [nm,rt] -> (nm,rate_parse (map toUpper rt))
      [nm] -> (nm,Nothing)
      _ -> error "ugen_sep"

-- > ugen_io "SinOsc" == Just (2,1)
-- > mapMaybe ugen_io ["Out","ResonZ","Pan2","Drand"]
-- > mapMaybe ugen_io ["BrownNoise","Dust","LFNoise0","LFNoise1","Rand"]
-- > mapMaybe ugen_io ["Max"]
-- > mapMaybe ugen_io ["In"]
ugen_io :: String -> Maybe (Int,Maybe Int)
ugen_io u =
    case DB.uLookup u of
      Just r -> Just (length (DB.ugen_inputs r)
                     ,either Just (const Nothing) (DB.ugen_outputs r))
      _ -> Nothing

-- there are name overlaps
--
-- - is a uop (negate) and a binop (minus)
-- Rand is a UGen and a uop
--
-- Max is a UGen (SLUGens) and a binop...

-- > map is_uop (words "Abs MIDICPS Neg")
-- > map is_uop (words "- Rand")
is_uop :: String -> Bool
is_uop s = s `notElem` ["-","Rand"] && isJust (unaryIndex s)

-- > map is_binop (words "== > % Trunc")
is_binop :: String -> Bool
is_binop s = s `notElem` [] && isJust (binaryIndex s)

-- * UForth

do_oscil :: String -> Rate -> Int -> Maybe Int -> U_Forth ()
do_oscil nm rt inp nc = do
  nc' <- case nc of
           Just n -> return n
           Nothing -> pop_int
  z <- if is_nondet nm then incr_id else return 0
  i <- pop_n inp
  let i' = (if u_halts_mce nm then halt_mce_transform else id) (reverse i)
  let gen = if is_nondet nm then nondet nm (UId z) else ugen nm
  push (gen rt i' nc')

do_filter :: String -> Int -> Maybe Int -> U_Forth ()
do_filter nm inp nc = do
  nc' <- case nc of
           Just n -> return n
           Nothing -> pop_int
  z <- if is_nondet nm then incr_id else return 0
  i <- pop_n inp
  let rt = maximum (map rateOf i)
      i' = (if u_halts_mce nm then halt_mce_transform else id) (reverse i)
      gen = if is_nondet nm then nondet nm (UId z) else ugen nm
  push (gen rt i' nc')

do_uop :: U_Reader
do_uop nm = do
  p <- pop
  let rt = rateOf p
  push (ugen_optimise_const_operator (uop nm rt p))

-- | This follows the ANS Forth convention, ie. @10 2 /@ is @5@.
do_binop :: U_Reader
do_binop nm = do
  p <- pop
  q <- pop
  let rt = max (rateOf p) (rateOf q)
  push (ugen_optimise_const_operator (binop nm rt q p))

-- | Order of lookup: binop, uop, ugen
do_ugen :: U_Reader
do_ugen u = do
  let (nm,rt) = ugen_sep u
  case is_binop nm of
    True -> do_binop nm
    False ->
        case is_uop nm of
          True -> do_uop nm
          False ->
              case ugen_io nm of
                Just (inp,outp) ->
                    case rt of
                      Just rt' -> do_oscil nm rt' inp outp
                      Nothing -> do_filter nm inp outp
                Nothing -> throwError (show ("do_ugen: unknown UGen",u))

ugen_dict :: Dict Int UGen
ugen_dict =
    M.fromList
    [("clone",pop_int >>= \n -> pop >>= \u -> incr_id >>= \z -> push (uclone z n u))
    ,("draw",pop >>= \u -> assert_empty >> liftIO (draw (out 0 u)))
    ,("mce",pop_int >>= \n -> pop_n n >>= \u -> push (mce (reverse u)))
    ,("mix",pop >>= push . mix)
    ,("mrg",pop_int >>= \n -> pop_n n >>= \u -> push (mrg (reverse u)))
    ,("play",pop >>= \u -> assert_empty >> liftIO (audition (out 0 u)))
    ,("stop",liftIO (withSC3 reset))
    ,("unmce",pop >>= \u -> push_l (mceChannels u))
    ,("pause",pop_double >>= \t -> pauseThread t)]

-- | Print as integer if integral, else as real.
real_pp :: (Show a, Real a) => a -> String
real_pp n =
    let r = toRational n
    in if denominator r == 1 then show (numerator r) else show n

-- | Print constants as numbers & primitives as names.
ugen_pp :: UGen -> String
ugen_pp u =
    case u of
      Constant_U (Constant n) -> real_pp n
      Primitive_U (Primitive _ nm _ _ sp _ ) -> "UGEN: " ++ ugen_user_name nm sp
      MCE_U (MCE_Unit u') -> ugen_pp u'
      MCE_U (MCE_Vector v) -> "[" ++ intercalate " " (map ugen_pp v) ++ "]"
      _ -> show u

instance Forth_Type UGen where
    ty_char = toEnum . (floor . u_constant)
    ty_string = ugen_pp
    ty_int = floor . u_constant
    ty_from_bool t = if t then -1 else 0
    ty_from_int = fromIntegral

parse_constant :: String -> Maybe UGen
parse_constant s =
    let d :: Maybe Double
        d = R.readMaybe s
    in fmap constant d

main :: IO ()
main = do
  let d :: Dict Int UGen
      d = M.unions [core_dict,show_dict,stack_dict,ugen_dict]
      vm = (empty_vm 0 parse_constant) {dynamic = Just do_ugen
                                       ,dict = d}
  dir <- lookupEnv "HSC3_FORTH_DIR"
  case dir of
    Nothing -> error "HSC3_FORTH_DIR NOT SET"
    Just dir' -> do
      putStrLn "HSC3-FORTH"
      vm' <- exec_err vm (fw_included' (dir' </> "stdlib.fs"))
      vm'' <- exec_err vm' (fw_included' (dir' </> "hsc3.fs"))
      vm''' <- exec_err vm'' (fw_included' (dir' </> "overlap-texture.fs"))
      repl vm''' {input_port = Just stdin}

{-
import Data.Boolean {- Boolean -}

instance Boolean UGen where
  true = -1
  false = 0
  notB n = if n /= 0 then 0 else -1
  p &&* q = if p /= 0 && q /= 0 then -1 else 0
  p ||* q = if p /= 0 || q /= 0 then -1 else 0

bool_ugen :: Bool -> UGen
bool_ugen t = if t then -1 else 0
-}

