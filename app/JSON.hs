{-# LANGUAGE ScopedTypeVariables #-}

module JSON ( JsonExpr
              ( JsonNull   , JsonBool
              , JsonNumber , JsonString
              , JsonArray  , JsonObject)
            , jsonZero , jsonTrue , jsonFalse
            , jsonEmptyString , jsonEmptyArray
            , jsonEmptyObject
            , parseJSON
            , parsePartialJSON) where

-- TODO: Definitely not an up-to-spec JSON parser,
-- but it should be enough for the 4chan API.

import Control.Applicative
import Data.Functor
import Text.ParserCombinators.ReadP

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.List (intercalate)

import Util

-- Datatype for a JSON parse-tree
data JsonExpr
  = JsonNull
  | JsonBool   Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray  [JsonExpr]
  | JsonObject (Map String JsonExpr)
  deriving Eq

-- Instances of `Show` and `Read` for `JsonExpr`.
instance Show JsonExpr where
  show JsonNull = "null"
  show (JsonBool True)  = "true"
  show (JsonBool False) = "false"
  show (JsonNumber f) = formatFloat f
  show (JsonString s) = show s
  show (JsonArray  a) = "["  ++ intercalate ", " (map show a) ++  "]"
  show (JsonObject m) = "{ " ++ intercalate ", " pair_strings ++ " }"
    where pair_strings = map (\p -> show (fst p) ++ ": " ++ show (snd p))
                             (Map.toList m)

instance Read JsonExpr where
  readsPrec _ = parsePartialJSON

-- Some default values
jsonZero  :: JsonExpr
jsonTrue  :: JsonExpr
jsonFalse :: JsonExpr
jsonEmptyString :: JsonExpr
jsonEmptyArray  :: JsonExpr
jsonEmptyObject :: JsonExpr

jsonZero  = JsonNumber 0
jsonTrue  = JsonBool True
jsonFalse = JsonBool False
jsonEmptyString = JsonString ""
jsonEmptyArray  = JsonArray  []
jsonEmptyObject = JsonObject Map.empty

-- Parsing `null'
readNull :: ReadP JsonExpr
readNull = string "null" >> pure JsonNull

-- Parsing Booleans
readBool :: ReadP JsonExpr
readBool = string "true" <|> string "false"
       <&> JsonBool . read . titleCase

-- Parsing numbers
-- TODO: Deciaml point, hexadecimal, octal, exponent notation, etc.
readNumber :: ReadP JsonExpr
readNumber = JsonNumber . read <$> many1 (digit 10)

-- Parsing strings
readString :: ReadP JsonExpr
readString = JsonString <$> stringLiteral

-- Parse anything delimitd by a certain string of characters,
-- and ending by a specific string.
readDelimited :: forall a. ReadP a -> String -> String -> ReadP [a]
readDelimited parser delimitor ending = collect [] <&> reverse
  where collect :: [a] -> ReadP [a]
        collect vs = do
          end <- option "" (skipSpaces >> string ending)
          if null end then do
            expr <- parser
            deli <- string delimitor <|> string ending
            if deli == ending
              then pure    (expr:vs)
              else collect (expr:vs)
          else return vs

-- Parsing JSON arrays
readArray :: ReadP JsonExpr
readArray = satisfy (== '[')
         >> JsonArray <$> readDelimited parseExpr "," "]"

-- Parsing JSON objects
readPair :: ReadP (String, JsonExpr)
readPair = do  -- This is only valid within a JSON object.
  key <- parseExpr
  _ <- satisfy (== ':')
  value <- parseExpr
  case key of -- Key mus have been a string.
    JsonString keyString -> return (keyString, value)
    _ -> pfail

readObject :: ReadP JsonExpr
readObject
   = satisfy (== '{') >> JsonObject . Map.fromList
                     <$> readDelimited readPair "," "}"

-- Parse any JSON expression
parseExpr :: ReadP JsonExpr
parseExpr
   =  spaced readNull
  <|> spaced readBool
  <|> spaced readNumber
  <|> spaced readString
  <|> spaced readArray
  <|> spaced readObject

-- Now actually parse strings...

-- Returns successful parses, and the rest of the unparsable JSON (if any).
parsePartialJSON :: ReadS JsonExpr
parsePartialJSON = readP_to_S parseExpr

-- Parse a JSON string.
-- A return value of Nothing suggests invalid JSON,
-- otherwise `Just` the parse-tree is returned.
-- (There is no error reporting, it's either valid or not.)
parseJSON :: String -> Maybe JsonExpr
parseJSON s = maybeHead [ expr | (expr,"") <- parsePartialJSON s ]
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead (h:_) = Just h

