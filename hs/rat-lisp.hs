import Lisp

main :: IO ()
main = do
  putStrLn "RAT-LISP"
  repl' =<< (env_toplevel :: IO (Env Rational))
