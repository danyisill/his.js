module Main where

import System.Environment
import System.Exit

import JSON
import Tests

data CommandFalg = Verbose
                 | RunTests
                 | DebugMode

main :: IO ()
main = do
  token <- lookupEnv "HIS_BOT_TOKEN"
  case token of
    Nothing -> putStrLn "[!!] No API token found."
    Just _ -> return ()

  args <- getArgs
  if any (== "--test") args
    then do putStrLn $ "All tests passed: " ++ show runTests
            exitWith $ if runTests then ExitSuccess else ExitFailure 1
    else return ()

  -- Remove this, its just for demonstration:
  putStrLn . show $ parse
    "  { \"a\": [null , 37, \"lol\", false ], \"b\" : true } "
