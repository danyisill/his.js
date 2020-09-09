-- Utility functions.

module Util where

import Control.Applicative
import Data.Functor

import qualified Data.Char as Char
import qualified Numeric
import GHC.Float

import Text.ParserCombinators.ReadP

digit :: Int -> ReadP Char
digit base = digit' where
  chrOff off chr = Char.chr $ Char.ord chr + off - 1
  digit'
    | base `elem` [2..10] = satisfy $ (flip elem) ['0' .. chrOff base '0']
    | base > 10 = satisfy $ (flip elem)
        (['0' .. '9'] ++ ['a' .. chrOff (base - 10) 'a']
                      ++ ['A' .. chrOff (base - 10) 'A'])
    | otherwise = error "base must be 2 or more."

spaces :: ReadP [Char]
spaces = many1 whitespace where
  whitespace :: ReadP Char
  whitespace =  char ' '
            <|> char '\n'
            <|> char '\t'
            <|> char '\r'
            <|> char '\xa0'
            <|> char '\HT'
            <|> char '\SP'
            <|> char '\VT'

spaced :: ReadP a -> ReadP a
spaced parser = do skipSpaces
                   expr <- parser
                   skipSpaces
                   return expr

stringBefore :: ReadP end -> ReadP String
stringBefore = manyTill (satisfy $ const True)

stringLiteral :: ReadP String
stringLiteral = readQuote >> manyTill readCharacters readQuote
  where readQuote :: ReadP Char
        readQuote = satisfy (== '"')
        readCharacters :: ReadP Char
        readCharacters = do
          chr <- get
          if chr == '\\' then do
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
          else return chr

titleCase :: String -> String
titleCase [] = []
titleCase (x:xs) = Char.toUpper x : map Char.toLower xs

lowerCase :: String -> String
lowerCase = map Char.toLower

upperCase :: String -> String
upperCase = map Char.toUpper

readHex :: String -> Int
readHex = fst . head . Numeric.readHex

formatFloat :: RealFloat a => a -> String
formatFloat f
  | f == 0 = "0"
  | abs f < 1e-6 || abs f > 1e20 = formatRealFloat FFExponent Nothing f
  | f - fromIntegral (floor f :: Int) == 0 = formatRealFloat FFFixed   (Just 0) f
  | otherwise = formatRealFloat FFGeneric  Nothing f


