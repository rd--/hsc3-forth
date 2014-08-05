import Control.Monad.Except {- mtl -}
import Data.Char
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}

import Sound.OSC {- hsc3 -}
import Sound.SC3.ID {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import Lisp

instance Lisp_Ty UGen where
    ty_show = ugen_concise_pp
    ty_from_bool t = if t then 1 else 0

lift_io :: IO () -> VM a (Cell a)
lift_io f = liftIO f >> return Nil

maybe_to_err :: String -> Maybe a -> VM a a
maybe_to_err msg = maybe (throwError msg) return

atom_err :: Cell a -> VM a a
atom_err = maybe_to_err "NOT ATOM?" . atom

atom_err' :: Cell a -> a
atom_err' = maybe (error "NOT ATOM?") id . atom

constant_err :: Cell UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "NOT CONSTANT?"

ugen_to_double :: String -> UGen -> Double
ugen_to_double c u =
    let err = error ("UGEN_TO_DOUBLE: " ++ c ++ ": " ++ show u)
        f = fromMaybe err . u_constant
    in f u

ugen_to_int :: String -> UGen -> Int
ugen_to_int c u =
    let err = error ("UGEN_TO_INT: " ++ c ++ ": " ++ show u)
        f = floor . fromMaybe err . u_constant
    in f u

mk_ugen :: Cell UGen -> UGen
mk_ugen c =
    let [nm,rt,inp,inp_mce,outp,sp,k] = to_list c
        inp_mce' = case inp_mce of
                     Atom u -> mceChannels u
                     _ -> []
        sp' = case sp of
                Atom u -> Special (ugen_to_int "SPECIAL" u)
                _ -> Special 0
        k' = case k of
               Atom u -> UId (ugen_to_int "UID" u)
               _ -> NoId
        inp' = mapMaybe atom (to_list inp) ++ inp_mce'
        outp' = floor (constant_err outp)
        rt' = case rt of
                Symbol sym -> fromJust (rate_parse (map toUpper sym))
                Cons _ _ ->
                    let f = rateOf . (inp' !!) . ugen_to_int "RATE" . atom_err'
                    in maximum (map f (to_list rt))
                _ -> error ("UNKNOWN RATE INPUT: " ++ show rt)
        nm' = case nm of
                String str -> str
                _ -> error ("UGEN NAME NOT STRING: " ++ show nm)
    in ugen_optimise_const_operator (mk_plain rt' nm' inp' outp' sp' k')

l_is_number :: Cell UGen -> Cell UGen
l_is_number c =
    case c of
      Atom u -> if isConstant u then l_true else l_false
      _ -> l_false

l_is_procedure :: Cell UGen -> Cell UGen
l_is_procedure c =
    case c of
      Fun _ -> l_true
      Proc _ -> l_true
      Lambda _ _ _ -> l_true
      Macro _ -> l_true
      _ -> l_false

l_clone_star :: Cell UGen -> VM UGen (Cell UGen)
l_clone_star c =
    case to_list c of
      [Atom k,Atom n,Atom u] ->
         let k' = ugen_to_int "CLONE-K" k
             n' = ugen_to_int "CLONE-N" n
         in return (Atom (uclone k' n' u))
      _ -> throwError ("clone*: " ++ show c)

l_play_at_star :: Cell UGen -> VM UGen (Cell UGen)
l_play_at_star c =
    case to_list c of
     [_,Atom u,Atom nid,Atom act,Atom grp] ->
         let nid' = ugen_to_int "PLAY-AT: NID" nid
             act' = ugen_to_int "PLAY-AT: ACT" act
             grp' = ugen_to_int "PLAY-AT: GRP" grp
         in lift_io (withSC3 (play_at (nid',toEnum act',grp') u)) >>
            return Nil
     _ -> throwError ("play-at*: " ++ show c)

l_thread_sleep :: Cell UGen -> VM UGen (Cell UGen)
l_thread_sleep c = do
    u <- atom_err c
    liftIO (pauseThread (ugen_to_double "pause" u))
    return Nil

ugen_dict :: Dict UGen
ugen_dict =
    M.fromList
    [("mk-ugen",Fun (Atom . mk_ugen))
    ,("number?",Fun l_is_number)
    ,("procedure?",Fun l_is_procedure)
    ,("clone*",Proc l_clone_star)
    ,("mce",Fun (\c -> Atom (mce (map atom_err' (to_list c)))))
    ,("mce-channels",Fun (\c -> from_list (map Atom (mceChannels (atom_err' c)))))
    ,("mrg",Fun (\c -> Atom (mrg (map atom_err' (to_list c)))))
    ,("draw",Proc (\c -> atom_err c >>= \u -> lift_io (draw (out 0 u))))
    ,("play-at*",Proc l_play_at_star)
    ,("stop",Proc (\_ -> lift_io (withSC3 reset)))
    ,("thread-sleep",Proc l_thread_sleep)
    ,("utcr",Proc (\_ -> liftIO time >>= return . Atom . constant))
    ,("sc3-status",Proc (\_ -> lift_io (withSC3 serverStatus >>= mapM_ putStrLn)))]

main :: IO ()
main = do
  putStrLn "HSC3-LISP"
  env <- gen_toplevel (M.unions [core_dict,ugen_dict]) :: IO (Env UGen)
  let nm = ["stdlib.lisp","rhs.lisp","hsc3.lisp","ugen.lisp","rsc3-compat.lisp","rsc3.lisp"]
  repl env (load_files nm)

{-

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Bindings.Lisp as DB {- hsc3-db -}

let b = map (\nm -> DB.gen_mk_ugen nm) DB.complete_names
writeFile "/home/rohan/sw/hsc3-forth/lisp/ugen.lisp" (unlines b)

-}
