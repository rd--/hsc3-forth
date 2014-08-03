import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import System.IO {- base -}

import Sound.OSC {- hosc -}
import Sound.SC3.ID {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import Forth
import SC3

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

pop_double :: Forth w UGen Double
pop_double =
    let f u = case constant_opt u of
                Nothing -> throw_error "POP_DOUBLE"
                Just n -> return n
    in pop >>= f

pop_int :: Forth w UGen Int
pop_int = fmap floor pop_double

-- | Get UId counter.
get_uid :: Forth Int a Int
get_uid = with_vm (\vm -> (vm,world vm))

-- | Store UId counter.
set_uid :: Int -> Forth Int a ()
set_uid k = with_vm (\vm -> (vm {world = k},()))

-- | Get UId counter and store increment.
incr_uid :: Forth Int a Int
incr_uid = with_vm (\vm -> let k = world vm in (vm {world = k + 1},k))

-- | Assert that the stack is empty.
fw_assert_empty :: Forth_Type a => Forth w a ()
fw_assert_empty = do
  vm <- get
  case stack vm of
    [] -> return ()
    l -> throw_error ("STACK NOT EMPTY: " ++ unwords (map show l))

ugen_sep' :: String -> Forth w a (String,Maybe Rate)
ugen_sep' = maybe (throw_error "UGEN NAME RATE SEPARATOR FAILED") return . ugen_sep

-- * UForth

get_nc :: Maybe Int -> U_Forth Int
get_nc nc =
    case nc of
      Just n -> return n
      Nothing -> pop_int

gen_plain :: U_Reader
gen_plain w = do
  (nm,rt) <- ugen_sep' w
  let (nm',sp) = case rt of
                   Nothing -> resolve_operator nm
                   _ -> (nm,Nothing) -- Rand.ir is UGen
      sp' = Special (fromMaybe 0 sp)
  u <- case ugen_rec nm' of
         Nothing -> throw_error ("DYNAMIC FAILED: UNKNOWN UGEN: " ++ tick_quotes nm')
         Just r -> return r
  when (isNothing rt && isNothing (DB.ugen_filter u))
       (throw_error ("OSC: NO RATE?: " ++ tick_quotes nm'))
  let (inp,nc) = ugen_io u
  z <- if DB.ugen_nondet u then fmap UId incr_uid else return NoId
  nc' <- get_nc nc
  i <- pop_n inp
  let rt' = fromMaybe (maximum (map rateOf i)) rt
      i' = (if u_halts_mce u then halt_mce_transform else id) (reverse i)
  push (mk_plain rt' nm' i' nc' sp' z)

sched :: Time -> UGen -> IO ()
sched t u =
    let nm = show (hashUGen u)
        sy = synthdef nm (out 0 u)
        b0 = bundle immediately [d_recv sy]
        b1 = bundle t [s_new nm (-1) AddToHead 1 []]
    in withSC3 (sendBundle b0 >> sendBundle b1)

fw_help :: Forth_Type a => Forth w a ()
fw_help = do
  (nm,_) <- ugen_sep' =<< pop_string
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
  liftIO (audition_at (nid,toEnum act,grp) u)

fw_see :: U_Forth ()
fw_see = pop_int >>= \k -> pop >>= \u -> liftIO (putStrLn (ugen_graph_forth_pp (toEnum k) u))

ugen_dict :: Dict Int UGen
ugen_dict =
    M.fromList
    [("clone",pop_int >>= \n -> pop >>= \u -> incr_uid >>= \z -> push (uclone z n u))
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
    ,("get-uid",get_uid >>= push . constant)
    ,("set-uid",pop_int >>= set_uid)
    ,("unrand",pop >>= push . ugen_optimise_ir_rand)
    ,("chan",pop >>= push . constant . length . mceChannels)
    ,("sc3-status",liftIO (withSC3 serverStatus >>= mapM_ putStrLn))
    ,("see",fw_see)
    ,("?",fw_help)]

instance Forth_Type UGen where
    ty_show = ugen_concise_pp
    ty_to_int = floor . fromMaybe (error "TY_TO_INT") . u_constant
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Dict Int UGen
      d = M.unions [core_dict,ugen_dict]
      vm = (empty_vm 0 parse_constant sig) {dynamic = Just gen_plain
                                           ,dict = d}
      init_f = load_files ["stdlib.fs","hsc3.fs","overlap-texture.fs"]
  putStrLn "HSC3-FORTH"
  repl (vm {input_port = Just stdin}) init_f
