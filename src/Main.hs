module Main where

import System.Environment
import System.Exit

import JSON
import Boards
import Tests

import Control.Monad (when, forM_)
import Data.Maybe (fromMaybe)
import Data.Set (Set, insert, member)
import qualified Data.Set as Set

data CommandFlag = Verbose
                 | RunTests
                 | DebugMode
                 | Board String
                 deriving (Eq, Ord)

collectFlags :: IO (Set CommandFlag)
collectFlags = collect Set.empty <$> getArgs
  where collect :: Set CommandFlag -> [String] -> Set CommandFlag
        collect set [] = set
        collect set (arg:args) = let
          flag = case arg of
            "-v"      -> Verbose
            "--test"  -> RunTests
            "--debug" -> DebugMode
            board     -> Board arg
          in collect (insert flag set) args

main :: IO ()
main = do
  token <- fromMaybe "" <$> lookupEnv "HIS_BOT_TOKEN"
  when (null token)
    $ putStrLn "[!!] No API token found."

  args <- collectFlags
  when (RunTests `member` args) $ do
    tests <- runTests
    putStrLn $ "All tests passed: " ++ show tests
    exitWith $ if tests then ExitSuccess else ExitFailure 1

  let boards = [ b | Board b <- Set.toList args ]

  forM_ boards
    $ \board -> sendMessages <$> fetchReplies board
