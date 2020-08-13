{-# LANGUAGE ScopedTypeVariables #-}

module JSON where
-- TODO: Definitely not an up-to-spec JSON parser,
-- but it should be enough for the 4chan API (maybe).

import Control.Applicative
import Text.ParserCombinators.ReadP

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Char as Char

titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = Char.toUpper x : map Char.toLower xs

data JsonExpr
  = JsonNull
  | JsonBool   Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray  [JsonExpr]
  | JsonObject (Map String JsonExpr)
  deriving (Show, Eq)

-- Parsing `null'
readNull :: ReadP JsonExpr
readNull = string "null" >> return JsonNull

-- Parsing Booleans
readBool :: ReadP JsonExpr
readBool = string "true" <|> string "false"
       >>= return . JsonBool . read . titleCase

-- Parsing numbers
-- TODO: Deciaml point, hexadecimal, octal, exponent notation, etc.
digit :: ReadP Char
digit = satisfy $ (flip elem) ['0'..'9']

readNumber :: ReadP JsonExpr
readNumber = many1 digit >>= return . JsonNumber . read

-- Parsing strings
-- TODO:
--  - Common escapes not supported (`\n', `\r', `\t', `\b', `\"', etc.).
--  - Hex byte escapes not supported (`\x00`).
--  - Unicode escapes not supported (`\u0000').
--  - And probably more...
readString :: ReadP JsonExpr
readString = readQuote
          >> manyTill (satisfy $ const True) readQuote
         >>= return . JsonString
  where readQuote = satisfy (== '"')

-- Parse anything delimitd by a certain string of characters,
-- and ending by a specific string.
-- TODO: Allow (and ignore) trailing delimitor (e.g [1,2,3,]).
readDelimited :: forall a. ReadP a -> String -> String -> ReadP [a]
readDelimited parser delimitor ending = collect [] >>= return . reverse
    where collect :: [a] -> ReadP [a]
          collect vs = do
            exp <- parser
            del <- string delimitor <|> string ending
            if del == ending
              then return (exp:vs)
              else do rest <- collect (exp:vs)
                      return rest

-- Parsing JSON arrays
readArray :: ReadP JsonExpr
readArray = satisfy (== '[')
         >> readDelimited parseExpr "," "]"
        >>= return . JsonArray

-- Parsing JSON objects
readPair :: ReadP (String, JsonExpr)
readPair = do  -- This is only valid within a JSON object.
  skipSpaces
  key <- readString
  skipSpaces
  satisfy (== ':')
  value <- parseExpr
  case key of
    JsonString keyString -> return (keyString, value)
    _ -> pfail

readObject :: ReadP JsonExpr
readObject
  = satisfy (== '{') >> readDelimited readPair "," "}"
  >>= return . JsonObject . Map.fromList

-- Parse any JSON expression
parseExpr :: ReadP JsonExpr
parseExpr = spaced readNull
        <|> spaced readBool
        <|> spaced readNumber
        <|> spaced readString
        <|> spaced readArray
        <|> spaced readObject
        -- Any JSON expression may be wrapped in however many spaces.
        where spaced :: ReadP a -> ReadP a
              spaced parser = do skipSpaces
                                 expr <- parser
                                 skipSpaces
                                 return expr

-- A return value of Nothing suggests invalid JSON.
-- (I'm too lazy to do error reporting, it's either valid or not.)
parse :: String -> Maybe JsonExpr
parse s = maybeHead [ expr | (expr, "") <- readP_to_S parseExpr s ]
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead (x:xs) = Just x

