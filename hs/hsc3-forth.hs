import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Char {- hash -}
import Data.Hashable {- hash -}
import Data.Maybe {- base -}
import System.IO {- base -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Sound.OSC as OSC {- hosc -}

import qualified Sound.SC3 as SC3 {- hsc3 -}
import qualified Sound.SC3.Common.Base as Base {- hsc3 -}
import qualified Sound.SC3.Common.Help as Help {- hsc3 -}
import qualified Sound.SC3.UGen.Plain as Plain {- hsc3 -}
import qualified Sound.SC3.UGen.PP as PP {- hsc3 -}

import qualified Sound.SC3.UGen.Protect as Protect {- hsc3-rw -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.PP as DB {- hsc3-db -}

import qualified Sound.SC3.UGen.Dot as Dot {- hsc3-dot -}

import qualified Forth {- hsc3-forth -}

-- * Forth

-- | hsc3-forth word.
type U_Forth r = Forth.Forth Int SC3.UGen r

-- | 'replicateM' of 'pop'.
pop_n :: Int -> Forth.Forth w a [a]
pop_n n = replicateM n Forth.pop

-- | 'mapM_' of 'push'.
push_l :: [a] -> Forth.Forth w a ()
push_l = mapM_ Forth.push

pop_double :: String -> Forth.Forth w SC3.UGen Double
pop_double msg =
    let f u = case SC3.constant_opt u of
                Nothing -> Forth.throw_error ("POP_DOUBLE: " ++ msg ++ ": " ++ show u)
                Just n -> return n
    in Forth.pop >>= f

-- | Get UId counter.
get_uid :: Forth.Forth Int a Int
get_uid = Forth.with_vm (\vm -> (vm,Forth.world vm))

-- | Store UId counter.
set_uid :: Int -> Forth.Forth Int a ()
set_uid k = Forth.with_vm (\vm -> (vm {Forth.world = k},()))

-- | Get UId counter and store increment.
incr_uid :: Forth.Forth Int a Int
incr_uid = Forth.with_vm (\vm -> let k = Forth.world vm in (vm {Forth.world = k + 1},k))

-- | Assert that the stack is empty.
fw_assert_empty :: Forth.Forth_Type a => Forth.Forth w a ()
fw_assert_empty = do
  vm <- get
  case Forth.stack vm of
    [] -> return ()
    l -> Forth.throw_error ("STACK NOT EMPTY: " ++ unwords (map show l))

ugen_sep :: String -> Forth.Forth w a (String,Maybe SC3.Rate)
ugen_sep = maybe (Forth.throw_error "UGEN NAME RATE SEPARATOR FAILED") return . SC3.sc3_ugen_name_sep

-- * UForth

-- DEMAND makes this two stage, since inputs are unknown until we know if to pop NC.
get_nc :: DB.U -> Maybe Int -> U_Forth (Maybe Int)
get_nc u nc =
    case nc of
      Just n -> return (Just n)
      Nothing -> case DB.ugen_nc_mce u of
                   Just _ -> return Nothing
                   Nothing -> fmap Just (Forth.pop_int "GET_NC")

-- > fmap ugen_io (DB.u_lookup Base.CI "DSEQ")
-- > fmap ugen_io (DB.u_lookup Base.CI "DEMAND")
ugen_io :: DB.U -> (Int,Maybe Int)
ugen_io u = (length (DB.ugen_inputs u),DB.u_fixed_outputs u)

gen_plain :: String -> U_Forth ()
gen_plain w = do
  (nm,rt) <- ugen_sep w
  let (nm',sp) = case rt of
                   Nothing -> SC3.resolve_operator Base.CI nm
                   _ -> (nm,Nothing)
      sp' = SC3.Special (fromMaybe 0 sp)
  u <- case DB.u_lookup Base.CI nm' of
         Nothing -> Forth.throw_error ("DYNAMIC FAILED: UNKNOWN UGEN: " ++ Forth.tick_quotes nm')
         Just r -> return r
  when (isNothing rt && isNothing (DB.ugen_filter u))
       (Forth.throw_error ("OSC: NO RATE?: " ++ Forth.tick_quotes nm'))
  let (inp,nc) = ugen_io u
  z <- if DB.ugen_nondet u then fmap SC3.UId incr_uid else return SC3.NoId
  nc' <- get_nc u nc
  i <- fmap reverse (pop_n inp)
  let nc'' = fromMaybe (length (SC3.mceChannels (last i))) nc'
  let rt' = fromMaybe (maximum (map SC3.rateOf i)) rt
      i' = (if DB.ugen_std_mce u  > 0 then SC3.halt_mce_transform else id) i
  Forth.push (SC3.ugen_optimise_const_operator (Plain.mk_plain rt' (DB.ugen_name u) i' nc'' sp' z))

gen_nm :: SC3.UGen -> String
gen_nm = show . hash . show

sched :: OSC.Time -> SC3.UGen -> IO ()
sched t u =
    let nm = gen_nm u
        sy = SC3.synthdef nm (SC3.out 0 u)
        b0 = OSC.bundle OSC.immediately [SC3.d_recv sy]
        b1 = OSC.bundle t [SC3.s_new nm (-1) SC3.AddToHead 1 []]
    in SC3.withSC3 (OSC.sendBundle b0 >> OSC.sendBundle b1)

fw_help :: Forth.Forth w a ()
fw_help = do
  (nm,_) <- ugen_sep =<< Forth.pop_string "HELP: NAME"
  case DB.ugen_summary_maybe Base.CI nm of
    Nothing -> Forth.throw_error ("?: NO HELP: " ++ nm)
    Just (_,h) -> liftIO (putStrLn h)

fw_manual :: Forth.Forth w a ()
fw_manual = do
  (nm,_) <- ugen_sep =<< Forth.pop_string "MANUAL: NAME"
  case DB.u_lookup Base.CI nm of
    Nothing -> Forth.throw_error ("MANUAL: NO ENTRY: " ++ nm)
    Just u -> liftIO (Help.sc3_scdoc_help_open False (DB.ugen_name u))

dpans_id_to_url :: String -> String
dpans_id_to_url dpans_id =
  let dpans_sec = read (takeWhile isDigit dpans_id) :: Int
  in printf "http://forth.sourceforge.net/std/dpans/dpans%d.htm#%s" dpans_sec dpans_id

fw_dpans :: Forth.Forth w a ()
fw_dpans = do
  dpans_id <- Forth.pop_string "DPANS: ID"
  liftIO (void (rawSystem "x-www-browser" [dpans_id_to_url dpans_id]))

fw_play_at :: U_Forth ()
fw_play_at = do
  grp <- Forth.pop_int "PLAY-AT: GRP"
  act <- Forth.pop_int "PLAY-AT: ACT"
  nid <- Forth.pop_int "PLAY-AT: NID"
  u <- Forth.pop
  fw_assert_empty
  liftIO (SC3.audition_at SC3.sc3_default_udp (nid,toEnum act,grp,[]) u)

fw_write_synthdef :: U_Forth ()
fw_write_synthdef = do
  fn <- Forth.pop_string "WRITE-SYNTHDEF: FILE-NAME"
  nm <- Forth.pop_string "WRITE-SYNTHDEF: SYNTHDEF-NAME"
  u <- Forth.pop
  fw_assert_empty
  liftIO (SC3.synthdefWrite fn (SC3.synthdef nm (SC3.out (SC3.control SC3.KR "out" 0) u)))

fw_pretty_print :: U_Forth ()
fw_pretty_print = do
  k <- Forth.pop_int "PRETTY-PRINT"
  u <- Forth.pop
  liftIO (putStrLn (DB.ugen_graph_forth_pp (toEnum k,False) u))

fw_load_datum :: Char -> U_Forth OSC.Datum
fw_load_datum c =
    case c of
      'i' -> Forth.pop_int "LOAD-DATUM: I" >>= return . OSC.int32
      'f' -> pop_double "LOAD-DATUM: F" >>= return . OSC.float
      's' -> Forth.pop_string "LOAD-DATUM: S" >>= return . OSC.string
      _ -> Forth.throw_error ("LOAD-DATUM: UNKNOWN TYPE: " ++ Forth.tick_quotes [c])

fw_async :: U_Forth ()
fw_async = do
  nm <- Forth.pop_string "ASYNC: MESSAGE-NAME"
  ty <- Forth.pop_string "ASYNC: OSC-TYPE-SIG"
  param <- mapM fw_load_datum (reverse (tail ty))
  _ <- liftIO (SC3.withSC3 (SC3.async (OSC.message nm (reverse param))))
  return ()

ugen_dict :: Forth.Dict Int SC3.UGen
ugen_dict =
    Map.fromList $ map (\(nm,en) -> (map toLower nm,en))
    [("clone",Forth.pop_int "CLONE" >>= \n -> Forth.pop >>= \u -> incr_uid >>= \z -> Forth.push (Protect.uclone_all z n u))
    ,("draw",Forth.pop >>= \u -> fw_assert_empty >> liftIO (Dot.draw (SC3.out 0 u)))
    ,("mce",Forth.pop_int "MCE" >>= \n -> pop_n n >>= Forth.push . SC3.mce . reverse)
    ,("mix",Forth.pop >>= Forth.push . SC3.mix) -- here rather hsc3.fs to get sum_opt for graph comparisons...
    ,("mrg",Forth.pop_int "MRG" >>= \n -> pop_n n >>= Forth.push . SC3.mrg . reverse)
    ,("play-at",fw_play_at)
    ,("write-synthdef",fw_write_synthdef)
    ,("sched",pop_double "SCHED" >>= \t -> Forth.pop >>= \u -> fw_assert_empty >> liftIO (sched t u))
    ,("stop",liftIO (SC3.withSC3 SC3.reset))
    ,("unmce",Forth.pop >>= push_l . SC3.mceChannels)
    ,("async",fw_async)
    ,("pause",pop_double "PAUSE" >>= OSC.pauseThread)
    ,("time",liftIO OSC.time >>= Forth.push . SC3.constant)
    ,("label",Forth.pop_string "LABEL" >>= Forth.push . SC3.label)
    ,("get-uid",get_uid >>= Forth.push . SC3.constant)
    ,("set-uid",Forth.pop_int "SET-UID" >>= set_uid)
    ,("unrand",Forth.pop >>= Forth.push . SC3.ugen_optimise_ir_rand)
    ,("chan",Forth.pop >>= Forth.push . SC3.constant . length . SC3.mceChannels)
    ,("sc3-status",liftIO (SC3.withSC3 SC3.serverStatus >>= mapM_ putStrLn))
    ,("pretty-print",fw_pretty_print)
    ,("?",fw_help)
    ,("manual",fw_manual)
    ,("dpans",fw_dpans)]

instance Forth.Forth_Type SC3.UGen where
    ty_show = PP.ugen_concise_pp
    ty_to_int = fmap floor . SC3.u_constant
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Forth.Dict Int SC3.UGen
      d = Map.unions [Forth.core_dict,ugen_dict]
      vm = (Forth.empty_vm 0 SC3.parse_constant sig)
           {Forth.dynamic = Just gen_plain
           ,Forth.dict = d
           ,Forth.input_port = Just stdin}
      init_f = Forth.load_files ["stdlib.fs","hsc3.fs","overlap-texture.fs","sapf.fs"]
  putStrLn "HSC3-FORTH"
  Forth.repl vm init_f
