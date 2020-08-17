{-# LANGUAGE ScopedTypeVariables #-}

module JSON ( JsonExpr
              ( JsonNull   , JsonBool
              , JsonNumber , JsonString
              , JsonArray  , JsonObject)
            , jsonZero
            , jsonTrue , jsonFalse
            , jsonEmptyString
            , jsonEmptyArray
            , jsonEmptyObject
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
import Data.List
import GHC.Float

-- Utility functions
titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = Char.toUpper x : map Char.toLower xs

readHex :: String -> Int
readHex = fst . head . Numeric.readHex

formatFloat :: RealFloat a => a -> String
formatFloat f
  | f == 0                          = "0"
  | abs f < 1e-6 || abs f > 1e20    = formatRealFloat FFExponent Nothing f
  | f - fromIntegral (floor f) == 0 = formatRealFloat FFFixed   (Just 0) f
  | otherwise                       = formatRealFloat FFGeneric  Nothing f

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
jsonZero = JsonNumber 0
jsonTrue  = JsonBool True
jsonFalse = JsonBool False
jsonEmptyString = JsonString ""
jsonEmptyArray  = JsonArray  []
jsonEmptyObject = JsonObject Map.empty

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
readString
   = readQuote
  >> JsonString <$> manyTill readCharacters readQuote
  where readQuote :: ReadP Char
        readQuote = satisfy (== '"') <|> satisfy (== '\'')
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
              c -> pure c  -- \", \', \/, \\, etc.
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
          else return vs

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
parseExpr
   =  spaced readNull
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

-- Now actually parse strings...

-- Returns successful parses, and the rest of the unparsable JSON (if any).
parsePartialJSON :: ReadS JsonExpr
parsePartialJSON s = readP_to_S parseExpr s

-- Parse a JSON string.
-- A return value of Nothing suggests invalid JSON,
-- otherwise `Just` the parse-tree is returned.
-- (There is no error reporting, it's either valid or not.)
parseJSON :: String -> Maybe JsonExpr
parseJSON s = maybeHead [ expr | (expr,"") <- parsePartialJSON s ]
  where maybeHead :: [a] -> Maybe a
        maybeHead [] = Nothing
        maybeHead (x:xs) = Just x



