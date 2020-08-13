module Tests where

import JSON

import qualified Data.Map.Strict as Map

testJsonGeneral :: Bool
testJsonGeneral = let
  parsed = parse "  { \"a2\": [ null, 3 ] , \"bc\" : true } "
  manual = JsonObject $ Map.fromList
    [ ("a2", JsonArray [ JsonNull, JsonNumber 3.0 ])
    , ("bc", JsonBool True)
    ]
  in case parsed of
       Nothing   -> False  -- Fail!
       Just tree -> tree == manual

runTests :: Bool
runTests =
  let tests = [testJsonGeneral]
  in all id tests
