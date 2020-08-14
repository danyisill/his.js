module Tests where

import JSON
import Control.Monad
import qualified Data.Map.Strict as Map

testJsonGeneral :: Bool
testJsonGeneral = let
  parsed = parseJSON "\
    \{\
    \  \"hello\": \" world!\",\
    \  \"a2\": [ null, 37, false ],\
    \  \"bc\" : true\
    \}"

  manual = JsonObject $ Map.fromList
    [ ("hello", JsonString " world!")
    , ("a2", JsonArray [ JsonNull, JsonNumber 37.0, JsonBool False ])
    , ("bc", JsonBool True)
    ]
  in case parsed of
       Nothing   -> False  -- Fail!
       Just tree -> tree == manual

testJsonArray :: Bool
testJsonArray = let
  parsed = parseJSON "[{\"page\":1},{\"page\":2},{\"Test\":[1570]}]"
  manual = JsonArray [ JsonObject $ Map.fromList [("page", JsonNumber 1.0)]
                     , JsonObject $ Map.fromList [("page", JsonNumber 2.0)]
                     , JsonObject $ Map.fromList [("Test", JsonArray [JsonNumber 1570])]
                     ]
  in parsed == Just manual

testJsonEmptyObject :: Bool
testJsonEmptyObject = let
  parses = [parseJSON "{}", parseJSON "{ }", parseJSON " { } "]
  manual = JsonObject Map.empty
  in all (\x -> x == Just manual) parses

testJsonEmptyArray :: Bool
testJsonEmptyArray = let
  parses = [parseJSON "[]", parseJSON "[ ]", parseJSON " [ ] "]
  manual = JsonArray []
  in all (\x -> x == Just manual) parses

testJsonString :: Bool
testJsonString = let
  parsed = parseJSON "\"Hello\\t \\x61\\n \\/ \\W\\u006frld\\\"!\\\"\""
  manual = JsonString "Hello\t \x61\n / World\"!\""
  in parsed == Just manual

data TestDesc = TestDesc { name :: String, passed :: Bool }

runTests :: IO Bool
runTests = do
  let tests = [ TestDesc "General test" testJsonGeneral
              , TestDesc "Arrays with elements" testJsonArray
              , TestDesc "Objects with no keys" testJsonEmptyObject
              , TestDesc "Array with no elements" testJsonEmptyArray
              , TestDesc "String with some escapes" testJsonString
              ]
  forM_ tests $ \test -> do
    putStrLn $ " Tests: " ++ name test
    putStrLn $ "     Passed: " ++ show (passed test)
    putStrLn ""

  return $ all passed tests
