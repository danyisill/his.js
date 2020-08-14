{-# LANGUAGE ScopedTypeVariables #-}

module JSON ( JsonExpr
              ( JsonNull   , JsonBool
              , JsonNumber , JsonString
              , JsonArray  , JsonObject)
            , parseJSON
            , parsePartialJSON) where

-- TODO: Definitely not an up-to-spec JSON parser,
-- but it should be enough for the 4chan API (maybe).

import Control.Applicative
import Data.Functor
import Text.ParserCombinators.ReadP

import Data.Map.Strict (Map)
import qualified Numeric
import qualified Data.Map.Strict as Map
import qualified Data.Char as Char


titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = Char.toUpper x : map Char.toLower xs

readHex :: String -> Int
readHex = fst . head . Numeric.readHex

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
readNull = const JsonNull <$> string "null"

-- Parsing Booleans
readBool :: ReadP JsonExpr
readBool = string "true" <|> string "false"
       >>= pure . JsonBool . read . titleCase

-- Parsing numbers
-- TODO: Deciaml point, hexadecimal, octal, exponent notation, etc.
digit :: Int -> ReadP Char
digit base = digit' where
  chrOff chr = Char.chr $ Char.ord chr + base
  digit'
    | base <= 10 = satisfy $ (flip elem) ['0'..chrOff '9']
    | base  > 10 = satisfy $ (flip elem)
        (['0'..'9'] ++ ['a'..chrOff 'a'] ++ ['A'..chrOff 'A'])

readNumber :: ReadP JsonExpr
readNumber = JsonNumber . read <$> many1 (digit 10)

-- Parsing strings
readString :: ReadP JsonExpr
readString = readQuote
          >> JsonString <$> manyTill readCharacters readQuote
       where readQuote :: ReadP Char
             readQuote = satisfy (== '"')
             readCharacters :: ReadP Char
             readCharacters = do
               char <- get
               if char == '\\' then do
                 escp <- get
                 case escp of
                   -- Bytes escape
                   'u' -> count 4 (digit 16) <&> Char.chr . readHex
                   'x' -> count 2 (digit 16) <&> Char.chr . readHex
                   -- Single char
                   '0' -> pure '\0'
                   'a' -> pure '\a'
                   'b' -> pure '\b'
                   'f' -> pure '\f'
                   'n' -> pure '\n'
                   'r' -> pure '\r'
                   't' -> pure '\t'
                   'v' -> pure '\v'
                   c -> pure c  -- \", \', \\, etc.
               else return char
-- Parse anything delimitd by a certain string of characters,
-- and ending by a specific string.
readDelimited :: forall a. ReadP a -> String -> String -> ReadP [a]
readDelimited parser delimitor ending = collect [] >>= return . reverse
    where collect :: [a] -> ReadP [a]
          collect vs = do
            end <- option "" (skipSpaces >> string ending)
            if null end then do
              expr <- parser
              deli <- string delimitor <|> string ending
              if deli == ending
                then return $ expr:vs
                else collect (expr:vs)
            else
              return vs

-- Parsing JSON arrays
readArray :: ReadP JsonExpr
readArray = satisfy (== '[')
         >> JsonArray <$> readDelimited parseExpr "," "]"

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
   = satisfy (== '{') >> JsonObject . Map.fromList
                     <$> readDelimited readPair "," "}"

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

parsePartialJSON :: ReadS JsonExpr
parsePartialJSON s = readP_to_S parseExpr s

-- A return value of Nothing suggests invalid JSON.
-- (I'm too lazy to do error reporting, it's either valid or not.)
parseJSON :: String -> Maybe JsonExpr
parseJSON s = maybeHead [ expr | (expr,"") <- readP_to_S parseExpr s ]
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead (x:xs) = Just x



