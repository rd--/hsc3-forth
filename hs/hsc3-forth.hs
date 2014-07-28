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

-- | hsc3-forth word.
type U_Forth r = Forth Int UGen r

-- | hsc3-forth reader.
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

set_id :: Int -> Forth Int a ()
set_id k = with_vm (\vm -> (vm {world = k},()))

-- | Assert that the stack is empty.
fw_assert_empty :: Forth_Type a => Forth w a ()
fw_assert_empty = do
  vm <- get
  case stack vm of
    [] -> return ()
    l -> throw_error ("STACK NOT EMPTY: " ++ unwords (map ty_show l))

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
ugen_sep :: String -> (String,Maybe Rate)
ugen_sep u =
    case splitOn "." u of
      [nm,rt] -> (nm,rate_parse (map toUpper rt))
      [nm] -> (nm,Nothing)
      _ -> error "UGEN NAME RATE SEPARATOR FAILED"

ugen_rec :: String -> Maybe DB.U
ugen_rec = DB.uLookup

ugen_io :: DB.U -> (Int,Maybe Int)
ugen_io u =
    (length (DB.ugen_inputs u)
    ,case DB.ugen_nc_input u of
       Nothing -> DB.ugen_outputs u
       Just _ -> Nothing)

-- | SC3 has name overlaps.  '-' is suppressed as a uop (see 'negate')
-- in prefence to the binop ('-').  Likewise 'Rand' is suppressed as a
-- uop and allowed as a UGen.
--
-- > map is_uop (words "Abs MIDICPS Neg")
-- > map is_uop (words "- Rand")
is_uop :: String -> Bool
is_uop s = s `notElem` ["-","Rand"] && isJust (unaryIndex s)

-- | Max is a UGen (SLUGens) and a binop, we look for binops first.
--
-- > map is_binop (words "== > % Trunc Max")
is_binop :: String -> Bool
is_binop s = s `notElem` [] && isJust (binaryIndex s)

-- * UForth

get_nc :: Maybe Int -> U_Forth Int
get_nc nc =
    case nc of
      Just n -> return n
      Nothing -> pop_int

gen_osc :: DB.U -> Rate -> U_Forth ()
gen_osc u rt = do
  let (inp,nc) = ugen_io u
      nm = DB.ugen_name u
  z <- if DB.ugen_nondet u then incr_id else return 0
  nc' <- get_nc nc
  i <- pop_n inp
  let i' = (if u_halts_mce u then halt_mce_transform else id) (reverse i)
  let gen = if DB.ugen_nondet u then nondet nm (UId z) else ugen nm
  push (gen rt i' nc')

gen_filter :: DB.U -> U_Forth ()
gen_filter u = do
  let (inp,nc) = ugen_io u
      nm = DB.ugen_name u
  z <- if DB.ugen_nondet u then incr_id else return 0
  nc' <- get_nc nc
  i <- pop_n inp
  let rt = maximum (map rateOf i)
      i' = (if u_halts_mce u then halt_mce_transform else id) (reverse i)
      gen = if DB.ugen_nondet u then nondet nm (UId z) else ugen nm
  push (gen rt i' nc')

gen_uop :: U_Reader
gen_uop nm = do
  p <- pop
  let rt = rateOf p
  push (ugen_optimise_const_operator (uop nm rt p))

-- | This follows the ANS Forth convention, ie. @10 2 /@ is @5@.
gen_binop :: U_Reader
gen_binop nm = do
  p <- pop
  q <- pop
  let rt = max (rateOf p) (rateOf q)
  push (ugen_optimise_const_operator (binop nm rt q p))

-- | Order of lookup: binop, uop, ugen
gen_ugen :: U_Reader
gen_ugen w = do
  let (nm,rt) = ugen_sep w
  trace 2 ("GEN_UGEN: " ++ show (w,nm,rt))
  case is_binop nm of
    True -> gen_binop nm
    False ->
        case is_uop nm of
          True -> gen_uop nm
          False ->
              case ugen_rec nm of
                Nothing -> throw_error (show "UNKNOWN UGEN: '" ++ nm ++ "'")
                Just u ->
                    case rt of
                      Just rt' -> gen_osc u rt'
                      Nothing -> gen_filter u

sched :: Time -> UGen -> IO ()
sched t u =
    let nm = show (hashUGen u)
        sy = synthdef nm (out 0 u)
        b0 = bundle immediately [d_recv sy]
        b1 = bundle t [s_new nm (-1) AddToHead 1 []]
    in withSC3 (sendBundle b0 >> sendBundle b1)

fw_help :: Forth w a ()
fw_help = do
  (nm,_) <- fmap ugen_sep read_token
  case DB.ugenSummary' True nm of
    Nothing -> throw_error ("?: NO HELP: " ++ nm)
    Just h -> liftIO (putStrLn h)

fw_play_at :: U_Forth ()
fw_play_at = do
  grp <- pop_int
  act <- pop_int
  nid <- pop_int
  u <- pop
  fw_assert_empty
  liftIO (audition_at (nid,toEnum act,grp) (out 0 u))

ugen_dict :: Dict Int UGen
ugen_dict =
    M.fromList
    [("clone",pop_int >>= \n -> pop >>= \u -> incr_id >>= \z -> push (uclone z n u))
    ,("draw",pop >>= \u -> fw_assert_empty >> liftIO (draw (out 0 u)))
    ,("mce",pop_int >>= \n -> pop_n n >>= push . mce . reverse)
    ,("mix",pop >>= push . mix)
    ,("mrg",pop_int >>= \n -> pop_n n >>= push . mrg . reverse)
    ,("play-at",fw_play_at)
    ,("sched",pop_double >>= \t -> pop >>= \u -> fw_assert_empty >> liftIO (sched t u))
    ,("stop",liftIO (withSC3 reset))
    ,("unmce",pop >>= push_l . mceChannels)
    ,("pause",pop_double >>= pauseThread)
    ,("time",liftIO time >>= push . constant)
    ,("label",pop_string >>= push . label)
    ,("seed",pop_int >>= set_id)
    ,("unrand",pop >>= push . ugen_optimise_ir_rand)
    ,("?",fw_help)]

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
      Label_U (Label s) -> show s
      Primitive_U (Primitive _ nm _ _ sp _ ) -> "UGEN:" ++ ugen_user_name nm sp
      MCE_U (MCE_Unit u') -> ugen_pp u'
      MCE_U (MCE_Vector v) -> "[" ++ intercalate " " (map ugen_pp v) ++ "]"
      _ -> show u

instance Forth_Type UGen where
    ty_show = ugen_pp
    ty_to_int = floor . u_constant
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

parse_constant :: String -> Maybe UGen
parse_constant s =
    let d :: Maybe Double
        d = R.readMaybe s
    in fmap constant d

main :: IO ()
main = do
  let d :: Dict Int UGen
      d = M.unions [core_dict,ugen_dict]
      vm = (empty_vm 0 parse_constant) {dynamic = Just gen_ugen
                                       ,dict = d}
  dir <- lookupEnv "HSC3_FORTH_DIR"
  case dir of
    Nothing -> error "HSC3_FORTH_DIR NOT SET"
    Just dir' -> do
      let nm = map (dir' </>) ["stdlib.fs","hsc3.fs","overlap-texture.fs"]
      vm' <- load_files nm vm
      putStrLn "HSC3-FORTH"
      repl vm' {input_port = Just stdin}
