import Control.Monad.State {- mtl -}
import Data.Char
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}

--import Sound.OSC {- hosc -}
import Sound.SC3.ID {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

--import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
--import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import Lisp
import SC3

instance Lisp_Ty UGen where
    ty_show = ugen_concise_pp
    ty_from_bool t = if t then 1 else 0

lift_io :: IO () -> VM a (Cell a)
lift_io f = liftIO f >> return Void

atom_err :: Cell a -> VM a a
atom_err = maybe_to_err "NOT ATOM?" . atom

constant_err :: Cell UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "NOT CONSTANT?"

mk_oscillator :: Cell UGen -> UGen
mk_oscillator c =
    let [nm,rt,inp,outp] = to_list c
        inp' = mapMaybe atom (to_list inp)
        outp' = floor (constant_err outp)
    in case (nm,rt) of
         (String nm',Symbol rt') -> ugen nm' (fromJust (rate_parse (map toUpper rt'))) inp' outp'
         _ -> error "mk_osc"

ugen_dict :: Dict UGen
ugen_dict =
    M.fromList
    [("mk-osc",Fun (Atom . mk_oscillator))
    ,("draw",Proc (\c -> atom_err c >>= \u -> lift_io (draw (out 0 u))))
    ,("play",Proc (\c -> atom_err c >>= \u -> lift_io (audition (out 0 u))))
    ,("stop",Proc (\_ -> lift_io (withSC3 reset)))
    ,("sc3-status",Proc (\_ -> lift_io (withSC3 serverStatus >>= mapM_ putStrLn)))]

main :: IO ()
main = do
  putStrLn "HSC3-LISP"
  env <- gen_toplevel (M.unions [core_dict,num_dict,float_dict,ugen_dict]) :: IO (Env UGen)
  repl env (load_files ["stdlib.lisp","rhs.lisp","hsc3.lisp"])
