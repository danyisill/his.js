{-# LANGUAGE FlexibleInstances #-}

module EscapeHTML where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Function
import Text.Read (readMaybe)
import Text.ParserCombinators.ReadP

import Util
import Data.Char as Char
import Data.List (intercalate)

data HtmlTagType = Html | Head | Body | Title |     P |       B | Strong
                 |    I |   Em |    U |   Ins |     S |  Strike | Del | Meta
                 |    A | Code |  Pre |   Div |    H1 |      H2 |  H3 | H4
                 |   H5 |   H6 |   Ul |    Ol |    Li | Marquee | Center
                 |   Br |  Img | Link |    Hr | Table |   Tbody | Td  | Span
                 |   Tr |   Th | Form | Input | Quote | Blockquote
                 -- &c., &c.
                 deriving (Show, Read, Eq)

data HtmlAttribute = HtmlAttribute String String deriving Eq
                                -- ^Name  ^Value
data HTML = HtmlPlain String
          | HtmlDeclaration String [HtmlAttribute]
          | HtmlTag HtmlTagType [HtmlAttribute] [HTML]
          deriving Eq

instance Show HtmlAttribute where
  show (HtmlAttribute key value) = key ++ "=" ++ show value

instance {-# OVERLAPPING #-} Show [HtmlAttribute] where
  show = intercalate " " . map show

instance Show HTML where
  show (HtmlPlain s) = s
  show (HtmlDeclaration t [])
    = "<" ++ lowerCase (show t) ++ ">"
  show (HtmlDeclaration t a)
    = "<" ++ lowerCase (show t) ++ " " ++ show a ++ ">"
  show (HtmlTag t [] c)
    = "<" ++ lowerCase (show t) ++ ">"
   ++ show c ++ "</" ++ lowerCase (show t) ++ ">"
  show (HtmlTag t a c)
    = "<" ++ lowerCase (show t) ++ " " ++ show a ++ ">"
   ++ show c ++ "</" ++ lowerCase (show t) ++ ">"

instance {-# OVERLAPPING #-} Show [HTML] where
  show elements = concat $ map show elements

-- Self-closing tags
voidTags :: [HtmlTagType]
voidTags = [ Br , Hr , Img , Meta , Input , Link ]

-- Valid HTML tags for Telegram messages.
telegramSafeTags :: [HtmlTagType]
telegramSafeTags = [ B   , Strong , I    , Em
                   , U   , Ins    , S    , Strike
                   , Del , A      , Code , Pre
                   ]

parseHtmlAttribute :: ReadP HtmlAttribute
parseHtmlAttribute = do
  name  <- stringBefore equals
  value <- stringLiteral
  return $ HtmlAttribute name value
  where equals = spaced $ satisfy (== '=')

parseHtmlDeclaration :: ReadP HTML
parseHtmlDeclaration = do
  _ <- string "<!" <|> string "<?"
  tag <- stringBefore spaces
  attributes <- many parseHtmlAttribute
  return $ HtmlDeclaration tag attributes

parseTag :: String -> ReadP HtmlTagType
parseTag s = case readMaybe $ titleCase s of
               Just tag -> pure tag
               Nothing  -> pfail

parseSelfClosingTag :: ReadP (String, Bool, [HTML] -> HTML)
parseSelfClosingTag = do
  _ <- char '<'
  skipSpaces
  tag <- stringBefore spaces
  attributes <- many1 $ spaced parseHtmlAttribute
  _ <- char '/'
  skipSpaces
  _ <- char '>'
  tagType <- parseTag tag
  return (tag, True, HtmlTag tagType attributes)

parseEmptySelfClosingTag :: ReadP (String, Bool, [HTML] -> HTML)
parseEmptySelfClosingTag = do
  _ <- char '<'
  skipSpaces
  tag <- stringBefore $ (many spaces >> char '/')
  _ <- char '>'
  when (any Char.isSpace tag) pfail
  tagType <- parseTag tag
  return (tag, True, HtmlTag tagType [])

parseOpenTag :: ReadP (String, Bool, [HTML] -> HTML)
parseOpenTag = do _ <- char '<'
                  skipSpaces
                  tag <- stringBefore spaces
                  attributes <- many1 $ spaced parseHtmlAttribute
                  _ <- char '>'
                  tagType <- parseTag tag
                  let isVoid = tagType `elem` voidTags
                  return (tag, isVoid, HtmlTag tagType attributes)

parseEmptyOpenTag :: ReadP (String, Bool, [HTML] -> HTML)
parseEmptyOpenTag = do _ <- char '<'
                       skipSpaces
                       tag <- stringBefore $ (many spaces >> char '>')
                       when (any Char.isSpace tag) pfail
                       tagType <- parseTag tag
                       let isVoid = tagType `elem` voidTags
                       return (tag, isVoid, HtmlTag tagType [])

parseHtmlTag :: ReadP HTML
parseHtmlTag = do
  (tag, closed, openTag) <- parseEmptyOpenTag
                        <++ parseEmptySelfClosingTag
                        <++ parseOpenTag
                        <++ parseSelfClosingTag
  if closed
    then openTag <$> pure []
    else openTag <$> manyTill parseHtml (endTag tag)
  where endTag tag = do _ <- char '<'
                        skipSpaces
                        _ <- char '/'
                        skipSpaces
                        _ <- string tag
                        skipSpaces
                        _ <- char '>'
                        return ()


parseHtmlPlain :: ReadP HTML
parseHtmlPlain = do s <- stringBefore tagStartOrEnd
                    if null s
                      then pfail
                      else pure $ HtmlPlain s
  where
  tagStartOrEnd = do
    rest <- look
    if null rest
      then return ""
      else case head rest of
        '<' -> return rest
        '>' -> return rest
        _ -> pfail

skipEndTag :: ReadP ()
skipEndTag = optional $ do
  _ <- char '<'
  skipSpaces
  _ <- char '/'
  skipSpaces
  stringBefore $ char '>'

parseHtml :: ReadP HTML
parseHtml = skipEndTag >> parseHtmlDeclaration
                      <++ parseHtmlTag
                      <++ parseHtmlPlain

parsePartialHtml :: ReadS HTML
parsePartialHtml = readP_to_S parseHtml

instance Read HTML where
  readsPrec _ = parsePartialHtml

parseHTML :: String -> HTML
parseHTML = read

-- Parse HTML with no root element.
parseManyHTML :: String -> [HTML]
parseManyHTML s = head [ html | (html,"") <- readP_to_S (many parseHtml) s ]

escapeHTML :: [HTML] -> String
-- ^ Filters out non-Telegram supported HTMl from text.
escapeHTML html = let
  -- <br> tags are replaced with "\n"
  brToNewline :: HTML -> HTML
  brToNewline (HtmlTag Br _ _) = HtmlPlain "\n"
  brToNewline (HtmlTag t a children) = HtmlTag t a $ map brToNewline children
  brToNewline h = h

  -- HTML attributes except safe ones (e.g. href) are removed.
  removeAttributes :: HTML -> HTML
  removeAttributes (HtmlTag t a c)
    = HtmlTag t (filter isSafe a) (map removeAttributes c)
    where isSafe :: HtmlAttribute -> Bool
          isSafe (HtmlAttribute "href" _) = True
          isSafe _ = False
  removeAttributes h = h

  -- Unknown/unsupported tags (e.g. <span>) are simply erased.
  unwrapUnknown :: HTML -> HTML
  unwrapUnknown (HtmlTag t a c)
    | t `elem` telegramSafeTags = HtmlTag t a $ map unwrapUnknown c
    | otherwise = HtmlPlain $ escapeHTML c
  unwrapUnknown h = h

  in map brToNewline html
   & map unwrapUnknown
   & map removeAttributes
   & map show
   & concat
