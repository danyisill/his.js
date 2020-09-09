module Tests where

import JSON
import EscapeHTML
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

testHtmlPlain :: Bool
testHtmlPlain = let
  parses = parseManyHTML "This is some plain text."
  expect = [HtmlPlain "This is some plain text."]
  in parses == expect

testHtmlSimple :: Bool
testHtmlSimple = let
  parses = parseManyHTML "Hello <i>World</i>."
  expect = [ HtmlPlain "Hello "
           , HtmlTag I [] [HtmlPlain "World"]
           , HtmlPlain "."
           ]
  in parses == expect

testHtmlAttributes :: Bool
testHtmlAttributes = let
  parses = parseManyHTML "Hi, <b color=\"red\" class=\"hi\">I'm bold</b>."
  expect = [ HtmlPlain "Hi, "
           , HtmlTag B [ HtmlAttribute "color" "red"
                       , HtmlAttribute "class" "hi"
                       ]
                       [HtmlPlain "I'm bold"]
           , HtmlPlain "."
           ]
  in parses == expect

testHtmlSelfCloseDefault :: Bool
testHtmlSelfCloseDefault = let
  parses = parseManyHTML "New<br>Line"
  expect = [HtmlPlain "New", HtmlTag Br [] [], HtmlPlain "Line"]
  in parses == expect

testHtmlSelfCloseDefaultAttributes :: Bool
testHtmlSelfCloseDefaultAttributes = let
  parses = parseManyHTML "Horizontal<hr class=\"thick\">Rule"
  expect = [ HtmlPlain "Horizontal"
           , HtmlTag Hr [HtmlAttribute "class" "thick"] []
           , HtmlPlain "Rule"
           ]
  in parses == expect

testHtmlSelfCloseForce :: Bool
testHtmlSelfCloseForce = let
  parses = parseManyHTML "Empty<p />Paragraph"
  expect = [HtmlPlain "Empty", HtmlTag P [] [], HtmlPlain "Paragraph"]
  in parses == expect

testHtmlSelfCloseForceAttributes :: Bool
testHtmlSelfCloseForceAttributes = let
  parses = parseManyHTML "Empty<a href=\"x\" class=\"hi\" />Link"
  expect = [ HtmlPlain "Empty"
           , HtmlTag A
              [HtmlAttribute "href" "x", HtmlAttribute "class" "hi"] []
           , HtmlPlain "Link"]
  in parses == expect

testHtmlRandomClose :: Bool
testHtmlRandomClose = let
  parses = parseManyHTML "</p>Nothing to</p>close"
  expect = [HtmlPlain "Nothing to", HtmlPlain "close"]
  in parses == expect

data TestDesc = TestDesc { name :: String, passed :: Bool }

runTests :: IO Bool
runTests = do
  let tests = [ TestDesc "JSON: General test" testJsonGeneral
              , TestDesc "JSON: Arrays with elements" testJsonArray
              , TestDesc "JSON: Objects with no keys" testJsonEmptyObject
              , TestDesc "JSON: Array with no elements" testJsonEmptyArray
              , TestDesc "JSON: String with some escapes" testJsonString
              , TestDesc "HTML: Plain text" testHtmlPlain
              , TestDesc "HTML: Simple tag and plain text" testHtmlSimple
              , TestDesc "HTML: Tag with attributes" testHtmlAttributes
              , TestDesc "HTML: Self closing tag" testHtmlSelfCloseDefault
              , TestDesc "HTML: Self closing tag attributes" testHtmlSelfCloseDefaultAttributes
              , TestDesc "HTML: Force self closing tag" testHtmlSelfCloseForce
              , TestDesc "HTML: Force self closing tag attributes" testHtmlSelfCloseForceAttributes
              , TestDesc "HTML: Random closing tags" testHtmlRandomClose
              ]
  forM_ tests $ \test -> do
    putStrLn $ " Tests: " ++ name test
    putStrLn $ "     Passed: " ++ show (passed test)
    putStrLn ""

  return $ all passed tests
