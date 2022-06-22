import Control.Concurrent {- base -}
import Control.Monad {- base -}
import Control.Monad.Except {- mtl -}
import Control.Monad.State {- mtl -}
import Data.Char {- base -}
import Data.Hashable {- hashable -}
import Data.Maybe {- base -}
import System.IO {- base -}
import System.Process {- process -}
import Text.Printf {- base -}

import qualified Data.Map as Map {- containers -}

import qualified Sound.Osc as Osc {- hosc -}

import qualified Sound.Sc3 as Sc3 {- hsc3 -}
import qualified Sound.Sc3.Common.Base as Base {- hsc3 -}
import qualified Sound.Sc3.Common.Help as Help {- hsc3 -}
import qualified Sound.Sc3.Ugen.Plain as Plain {- hsc3 -}
import qualified Sound.Sc3.Ugen.Pp as Pp {- hsc3 -}

import qualified Sound.Sc3.Ugen.Protect as Protect {- hsc3-rw -}

import qualified Sound.Sc3.Ugen.Db as Db {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Record as Db {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Pp as Db {- hsc3-db -}
import qualified Sound.Sc3.Ugen.Db.Pp.Graph as Db {- hsc3-db -}

import qualified Sound.Sc3.Ugen.Dot as Dot {- hsc3-dot -}

import qualified Language.Forth.Interpreter.Simple as Forth {- hsc3-forth -}

-- * Forth

-- | hsc3-forth word.
type U_Forth r = Forth.Forth Int Sc3.Ugen r

-- | 'replicateM' of 'Forth.pop'.
pop_n :: Int -> Forth.Forth w a [a]
pop_n n = replicateM n Forth.pop

-- | 'mapM_' of 'Forth.push'.
push_l :: [a] -> Forth.Forth w a ()
push_l = mapM_ Forth.push

-- | Type checking 'Forth.pop'
pop_double :: String -> Forth.Forth w Sc3.Ugen Double
pop_double msg =
    let f u = case Sc3.constant_opt u of
                Nothing -> Forth.throw_error ("pop_double: " ++ msg ++ ": " ++ show u)
                Just n -> return n
    in Forth.pop >>= f

-- | Get Uid counter.
get_uid :: Forth.Forth Int a Int
get_uid = Forth.with_vm (\vm -> (vm,Forth.world vm))

-- | Store Uid counter.
set_uid :: Int -> Forth.Forth Int a ()
set_uid k = Forth.with_vm (\vm -> (vm {Forth.world = k},()))

-- | Get Uid counter and store increment.
incr_uid :: Forth.Forth Int a Int
incr_uid = Forth.with_vm (\vm -> let k = Forth.world vm in (vm {Forth.world = k + 1},k))

-- | Assert that the stack is empty.
fw_assert_empty :: Forth.Forth_Type a => Forth.Forth w a ()
fw_assert_empty = do
  vm <- get
  case Forth.stack vm of
    [] -> return ()
    l -> Forth.throw_error ("Stack not empty: " ++ unwords (map show l))

-- | Allow Ugens to have a rate specified as a suffix to the name.
ugen_sep :: String -> Forth.Forth w a (String,Maybe Sc3.Rate)
ugen_sep = maybe (Forth.throw_error "Ugen name rate separator failed") return . Sc3.sc3_ugen_name_sep

-- * UForth

-- DEMAND makes this two stage, since inputs are unknown until we know if to pop NC.
get_nc :: Db.U -> Maybe Int -> U_Forth (Maybe Int)
get_nc u nc =
    case nc of
      Just n -> return (Just n)
      Nothing -> case Db.ugen_nc_mce u of
                   Just _ -> return Nothing
                   Nothing -> fmap Just (Forth.pop_int "Get_nc")

-- > fmap ugen_io (Db.u_lookup_ci "DSEQ")
-- > fmap ugen_io (Db.u_lookup_ci "DEMAND")
-- > fmap ugen_io (Db.u_lookup_ci "irand")
ugen_io :: Db.U -> (Int,Maybe Int)
ugen_io u = (length (Db.ugen_inputs u),Db.u_fixed_outputs u)

{- | If an oscillator is given without a rate suffix, provide the default rate.

> get_osc_def_rate "sinosc" == Just AudioRate
-}
get_osc_def_rate :: (String, Maybe Sc3.Rate) -> (String, Maybe Sc3.Rate)
get_osc_def_rate (nm, dotRt) =
  case dotRt of
    Nothing -> case Db.u_lookup_ci nm of
                 Nothing -> (nm, Nothing)
                 Just u -> if Db.u_is_filter u then (nm, Nothing) else (nm, Just (Db.ugen_default_rate u))
    _ -> (nm, dotRt)

gen_plain :: String -> U_Forth ()
gen_plain w = do
  (nm,rt) <- fmap get_osc_def_rate (ugen_sep w)
  let (nm',sp) = case rt of
                   Nothing -> Sc3.resolve_operator Base.CI nm
                   _ -> (nm,Nothing)
      sp' = Sc3.Special (fromMaybe 0 sp)
  u <- case Db.u_lookup_ci nm' of
         Nothing -> Forth.throw_error ("Dynamic failed: unknown Ugen: " ++ Forth.tick_quotes nm')
         Just r -> return r
  when (isNothing rt && isNothing (Db.ugen_filter u))
       (Forth.throw_error ("Osc: no rate?: " ++ Forth.tick_quotes nm'))
  let (inp,nc) = ugen_io u
  z <- if Db.ugen_nondet u then fmap Sc3.Uid incr_uid else return Sc3.NoId
  nc' <- get_nc u nc
  i <- fmap reverse (pop_n inp)
  let nc'' = fromMaybe (length (Sc3.mceChannels (last i))) nc'
  let rt' = fromMaybe (maximum (map Sc3.rateOf i)) rt
      i' = (if Db.ugen_std_mce u  > 0 then Sc3.halt_mce_transform else id) i
  Forth.push (Sc3.ugen_optimise_const_operator (Plain.mk_plain rt' (Db.ugen_name u) i' nc'' sp' z))

gen_nm :: Sc3.Ugen -> String
gen_nm = show . hash . show

sched :: Osc.Time -> Sc3.Ugen -> IO ()
sched t u =
    let nm = gen_nm u
        sy = Sc3.synthdef nm (Sc3.out 0 u)
        b0 = Osc.bundle Osc.immediately [Sc3.d_recv sy]
        b1 = Osc.bundle t [Sc3.s_new nm (-1) Sc3.AddToHead 1 []]
    in Sc3.withSc3 (Osc.sendBundle b0 >> Osc.sendBundle b1)

fw_help :: Forth.Forth w a ()
fw_help = do
  (nm,_) <- ugen_sep =<< Forth.pop_string "Help: name"
  case Db.ugen_summary_maybe Base.CI nm of
    Nothing -> Forth.throw_error ("?: no help: " ++ nm)
    Just (_,h) -> liftIO (putStrLn h)

fw_manual :: Forth.Forth w a ()
fw_manual = do
  (nm,_) <- ugen_sep =<< Forth.pop_string "Manual: name"
  case Db.u_lookup_ci nm of
    Nothing -> Forth.throw_error ("Manual: no entry: " ++ nm)
    Just u -> liftIO (Help.sc3_scdoc_help_open False (Db.ugen_name u))

dpans_id_to_url :: String -> String
dpans_id_to_url dpans_id =
  let dpans_sec = read (takeWhile isDigit dpans_id) :: Int
  in printf "http://forth.sourceforge.net/std/dpans/dpans%d.htm#%s" dpans_sec dpans_id

fw_dpans :: Forth.Forth w a ()
fw_dpans = do
  dpans_id <- Forth.pop_string "Dpans: id"
  liftIO (void (rawSystem "x-www-browser" [dpans_id_to_url dpans_id]))

fw_play_at :: U_Forth ()
fw_play_at = do
  grp <- Forth.pop_int "Play-at: grp"
  act <- Forth.pop_int "Play-at: act"
  nid <- Forth.pop_int "Play-at: nid"
  u <- Forth.pop
  fw_assert_empty
  liftIO (Sc3.auditionAt Sc3.sc3_default_udp (nid,toEnum act,grp,[]) u)

fw_write_synthdef :: U_Forth ()
fw_write_synthdef = do
  fn <- Forth.pop_string "Write-synthdef: file-name"
  nm <- Forth.pop_string "Write-synthdef: synthdef-name"
  u <- Forth.pop
  fw_assert_empty
  liftIO (Sc3.synthdefWrite fn (Sc3.synthdef nm (Sc3.out (Sc3.control Sc3.ControlRate "out" 0) u)))

fw_pretty_print :: U_Forth ()
fw_pretty_print = do
  k <- Forth.pop_int "Pretty-print"
  u <- Forth.pop
  liftIO (putStrLn (Db.ugen_graph_forth_pp (toEnum k,False) u))

fw_load_datum :: Char -> U_Forth Osc.Datum
fw_load_datum c =
    case c of
      'i' -> Forth.pop_int "Load-datum: i" >>= return . Osc.int32
      'f' -> pop_double "Load-datum: f" >>= return . Osc.float
      's' -> Forth.pop_string "Load-datum: s" >>= return . Osc.string
      _ -> Forth.throw_error ("Load-datum: unknown type: " ++ Forth.tick_quotes [c])

fw_async :: U_Forth ()
fw_async = do
  nm <- Forth.pop_string "Async: message-name"
  ty <- Forth.pop_string "Async: osc-type-sig"
  param <- mapM fw_load_datum (reverse (tail ty))
  _ <- liftIO (Sc3.withSc3 (Sc3.async (Osc.message nm (reverse param))))
  return ()

ugen_dict :: Forth.Dict Int Sc3.Ugen
ugen_dict =
    Map.fromList $ map (\(nm,en) -> (map toLower nm,en))
    [("clone",Forth.pop_int "Clone" >>= \n -> Forth.pop >>= \u -> incr_uid >>= \z -> Forth.push (Protect.uclone_all z n u))
    ,("draw",Forth.pop >>= \u -> fw_assert_empty >> liftIO (Dot.draw (Sc3.out 0 u)))
    ,("mce",Forth.pop_int "Mce" >>= \n -> pop_n n >>= Forth.push . Sc3.mce . reverse)
    ,("mix",Forth.pop >>= Forth.push . Sc3.mix) -- here rather hsc3.fs to get sum_opt for graph comparisons...
    ,("mrg",Forth.pop_int "Mrg" >>= \n -> pop_n n >>= Forth.push . Sc3.mrg . reverse)
    ,("playAt",fw_play_at)
    ,("writeSynthdef",fw_write_synthdef)
    ,("sched",pop_double "Sched" >>= \t -> Forth.pop >>= \u -> fw_assert_empty >> liftIO (sched t u))
    ,("stop",liftIO (Sc3.withSc3 Sc3.reset))
    ,("unmce",Forth.pop >>= push_l . Sc3.mceChannels)
    ,("async",fw_async)
    ,("pause",pop_double "Pause" >>= Osc.pauseThread)
    ,("time",liftIO Osc.time >>= Forth.push . Sc3.constant)
    ,("label",Forth.pop_string "Label" >>= Forth.push . Sc3.label)
    ,("get-uid",get_uid >>= Forth.push . Sc3.constant)
    ,("set-uid",Forth.pop_int "Set-Uid" >>= set_uid)
    ,("unrand",Forth.pop >>= Forth.push . Sc3.ugen_optimise_ir_rand)
    ,("chan",Forth.pop >>= Forth.push . Sc3.constant . length . Sc3.mceChannels)
    ,("sc3Status",liftIO (Sc3.withSc3 Sc3.serverStatus >>= mapM_ putStrLn))
    ,("prettyPrint",fw_pretty_print)
    ,("?",fw_help)
    ,("manual",fw_manual)
    ,("dpans",fw_dpans)]

instance Forth.Forth_Type Sc3.Ugen where
    ty_show = Pp.ugen_concise_pp
    ty_to_int = fmap floor . Sc3.u_constant
    ty_from_int = fromIntegral
    ty_from_bool t = if t then -1 else 0

main :: IO ()
main = do
  sig <- newMVar False
  let d :: Forth.Dict Int Sc3.Ugen
      d = Map.unions [Forth.core_dict,ugen_dict]
      vm = (Forth.empty_vm 0 Sc3.parse_constant sig)
           {Forth.dynamic = Just gen_plain
           ,Forth.dict = d
           ,Forth.input_port = Just stdin}
      init_f = Forth.load_files ["stdlib.fs","hsc3.fs","overlap-texture.fs","sapf.fs"]
  putStrLn "hsc3-forth"
  Forth.repl vm init_f
