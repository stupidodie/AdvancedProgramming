-- Very rudimentary test of Arithmetic. Feel free to replace completely

import Definitions
import Arithmetic

import Data.List (intercalate)
import System.Exit (exitSuccess, exitFailure)  -- for when running stand-alone

tests :: [(String, Bool)]
tests = [test1, test2, test3] where
  test1 = ("test1", evalSimple (Add (Cst 2) (Cst 2)) == 4)
  test2 = ("test2", evalFull (Let "a" (Cst 42) (Var "a")) initEnv == 42)
  test3 = ("test3", evalErr (Var "x") initEnv == Left (EBadVar "x"))

main :: IO ()
main =
  let failed = [name | (name, ok) <- tests, not ok]
  in case failed of
       [] -> do putStrLn "All tests passed!"
                exitSuccess
       _ -> do putStrLn $ "Failed tests: " ++ intercalate ", " failed
               exitFailure
