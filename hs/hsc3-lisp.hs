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

atom_err' :: Cell a -> a
atom_err' = maybe (error "NOT ATOM?") id . atom

constant_err :: Cell UGen -> Double
constant_err c =
    case fmap constant_opt (atom c) of
      Just (Just n) -> n
      _ -> error "NOT CONSTANT?"

ugen_to_int :: UGen -> Int
ugen_to_int = floor . fromMaybe (error "UGEN_TO_INT") . u_constant

mk_ugen :: Cell UGen -> UGen
mk_ugen c =
    let [nm,rt,inp,inp_mce,outp,sp,k] = to_list c
        inp_mce' = case inp_mce of
                     Atom u -> mceChannels u
                     _ -> []
        sp' = case sp of
                Atom u -> Special (ugen_to_int u)
                _ -> Special 0
        k' = case k of
               Atom u -> UId (ugen_to_int u)
               _ -> NoId
        inp' = mapMaybe atom (to_list inp) ++ inp_mce'
        outp' = floor (constant_err outp)
        rt' = case rt of
                Symbol sym -> fromJust (rate_parse (map toUpper sym))
                _ -> maximum (map rateOf inp')
        nm' = case nm of
                String str -> str
                _ -> error "UGEN NAME NOT STRING?"
    in mk_plain rt' nm' inp' outp' sp' k'

-- + clone
ugen_dict :: Dict UGen
ugen_dict =
    M.fromList
    [("mk-ugen",Fun (Atom . mk_ugen))
    ,("number?",Fun (\c -> case c of {Atom _ -> l_true; _ -> l_false})) -- check constant
    ,("mce",Fun (\c -> Atom (mce (map atom_err' (to_list c)))))
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

-- | mk-ugen name rate|#f inputs mce-input|#f nc special#f uid#f
--
-- > import qualified Sound.SC3.UGen.DB as DB {- hsc3-db -}
-- > let nm = map DB.ugen_name DB.ugenDB ++ uop_names ++ binop_names
-- > let b = map (\nm -> gen_plain nm) nm
-- > writeFile "/home/rohan/sw/hsc3-forth/lisp/ugen.lisp" (unlines b)
gen_plain :: String -> String
gen_plain nm =
  let (nm',sp) = resolve_operator nm
      u = case ugen_rec nm' of
         Nothing -> error ("UNKNOWN UGEN: " ++ nm')
         Just r -> r
      nc = ugen_fixed_outputs u
      z = if DB.ugen_nondet u then "(incr-uid 1)" else "#f"
      i = map DB.input_name (DB.ugen_inputs u)
      (i',mc) = if u_halts_mce u
                then fromMaybe (error "GEN-UGEN: HALT MCE TRANSFORM") (sep_last i)
                else (i,"#f")
      i_k = if null i' then "nil" else "(list " ++ unwords i' ++ ")"
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
                 ,"    (mk-ugen (list \"%s\" %s %s %s %s %s %s))))"])
         (sc3_name_to_lisp_name nm) (unwords (mcons nc_var (mcons rt_var i)))
         nm' rt_k i_k mc nc_k sp' z

-- * UTIL

mcons :: Maybe a -> [a] -> [a]
mcons e = case e of {Nothing -> id; Just e' -> (e' :)}
