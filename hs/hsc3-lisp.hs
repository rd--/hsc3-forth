import Control.Monad.Except {- mtl -}
--import Control.Monad.State {- mtl -}
import Data.Char
import qualified Data.Map as M {- containers -}
import Data.Maybe {- base -}
import Text.Printf

--import Sound.OSC {- hosc -}
import Sound.SC3.ID {- hsc3 -}
import Sound.SC3.UGen.Plain {- hsc3 -}
import Sound.SC3.UGen.PP {- hsc3 -}

import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
import qualified Sound.SC3.UGen.DB.Record as DB {- hsc3-db -}

import Sound.SC3.UGen.Dot {- hsc3-dot -}

import Lisp
import SC3

instance Lisp_Ty UGen where
    ty_show = ugen_concise_pp
    ty_from_bool t = if t then 1 else 0

lift_io :: IO () -> VM a (Cell a)
lift_io f = liftIO f >> return Nil

maybe_to_err :: String -> Maybe a -> VM a a
maybe_to_err msg = maybe (throwError msg) return

atom_err :: Cell a -> VM a a
atom_err = maybe_to_err "NOT ATOM?" . atom

constant_err :: Cell UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "NOT CONSTANT?"

mk_ugen :: Cell UGen -> UGen
mk_ugen c =
    let [nm,rt,inp,inp_mce,outp,sp,k] = to_list c
        inp' = mapMaybe atom (to_list inp)
        outp' = floor (constant_err outp)
    in case (nm,rt) of
         (String nm',Symbol rt') -> ugen nm' (fromJust (rate_parse (map toUpper rt'))) inp' outp'
         _ -> error "mk_ugen"

ugen_dict :: Dict UGen
ugen_dict =
    M.fromList
    [("mk-ugen",Fun (Atom . mk_ugen))
    ,("draw",Proc (\c -> atom_err c >>= \u -> lift_io (draw (out 0 u))))
    ,("play",Proc (\c -> atom_err c >>= \u -> lift_io (audition (out 0 u))))
    ,("stop",Proc (\_ -> lift_io (withSC3 reset)))
    ,("sc3-status",Proc (\_ -> lift_io (withSC3 serverStatus >>= mapM_ putStrLn)))]

main :: IO ()
main = do
  putStrLn "HSC3-LISP"
  env <- gen_toplevel (M.unions [core_dict,num_dict,float_dict,ugen_dict]) :: IO (Env UGen)
  repl env (load_files ["stdlib.lisp","rhs.lisp","hsc3.lisp","ugen.lisp"])

-- * UGEN BINDINGS

mcons e = case e of {Nothing -> id; Just e' -> (e' :)}

-- | mk-ugen name rate|#f inputs mce-input|#f nc special#f uid#f
--
-- > let b = map (\nm -> gen_plain nm False) (map DB.ugen_name DB.ugenDB)
-- > writeFile "/home/rohan/sw/hsc3-forth/lisp/ugen.lisp" (unlines b)
gen_plain :: String -> Bool -> String
gen_plain nm is_op =
  let (nm',sp) = if is_op then resolve_operator nm else (nm,Nothing)
      u = case ugen_rec nm' of
         Nothing -> error ("UNKNOWN UGEN: " ++ nm')
         Just r -> r
      (inp,nc) = ugen_io u
      z = if DB.ugen_nondet u then "(incr-uid)" else "#f"
      i = map DB.input_name (DB.ugen_inputs u)
      (i',mc) = if u_halts_mce u
                then fromMaybe (error "GEN-UGEN: HALT MCE TRANSFORM") (sep_last i)
                else (i,"#f")
      (rt_var,rt_k) = case DB.ugen_filter u of
                        Nothing -> (Just "rt","rt")
                        Just _ -> (Nothing,"#f")
      (nc_var,nc_k) = case nc of
                        Nothing -> (Just "nc","nc")
                        Just d -> (Nothing,show d)
      sp' = case sp of
              Nothing -> "#f"
              Just k -> show k
  in printf
         (concat ["(define %s\n"
                 ,"  (lambda (%s)\n"
                 ,"    (mk-ugen (list \"%s\" %s (list %s) %s %s %s %s))))"])
         nm (unwords (mcons nc_var (mcons rt_var i)))
         nm' rt_k (unwords i') mc nc_k sp' z
