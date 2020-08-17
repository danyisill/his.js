{-# LANGUAGE OverloadedStrings #-}

module Boards (fetchReplies, sendMessages) where

import JSON
import EscapeHTML

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Network.HTTP.Simple  -- Dependency.
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Maybe (fromMaybe)
import Numeric

showFFixed f = showFFloat (Just 0) f ""

data TelegramMessage = TelegramMessage
  { caption :: String
  , media   :: String
  , channel :: String
  } deriving (Show, Eq)

fetchReplies :: String -> IO [TelegramMessage]
fetchReplies board = do
  putStrLn $ "Board: " ++ board

  putStrLn $ "Requesting: " ++ catalog
  res <- parseRequest catalog >>= httpLBS
  putStrLn $ "Status:  " ++ show (getResponseStatusCode res)
  putStrLn $ "Content: " ++ show (getResponseHeader "Content-Type" res)

  let resBody :: String
      resBody = L8.unpack $ getResponseBody res

  let json :: JsonExpr
      json = read resBody  -- Use `readMaybe` in case of failure.

  let (JsonArray pages) = json
  let messages = concat $ map distil pages

  return messages

  where catalog :: String
        catalog = "http://a.4cdn.org/" ++ board ++ "/catalog.json"

        fiftyOrMore :: JsonExpr -> Bool
        fiftyOrMore (JsonObject thread) = let
          replies = Map.lookup "replies" thread
          in case replies of
            Just (JsonNumber n) -> n > 50
            Nothing             -> False
        fiftyOrMore _ = False

        formMessage :: JsonExpr -> TelegramMessage
        formMessage (JsonObject thread) = let
          threadKey = flip Map.lookup thread
          -- Will exist:
          Just (JsonNumber  no) = threadKey  "no"
          Just (JsonNumber tim) = threadKey "tim"
          Just (JsonString ext) = threadKey "ext"
          -- May not exist:
          JsonString sub = fromMaybe jsonEmptyString $ threadKey "sub"
          JsonString com = fromMaybe jsonEmptyString $ threadKey "com"

          url = "https://boards.4channel.org/"
                ++ board ++ "/thread/" ++ showFFixed no
          caption = url ++ "\n" ++ escapeHTML (sub ++ "\n" ++ com)
          media = "https://is2.4chan.org/" ++ board ++ "/"
                  ++ showFFixed tim ++ ext
          channel = "@" ++ board ++ "50replies"
          in TelegramMessage caption media channel

        distil :: JsonExpr -> [TelegramMessage]
        distil (JsonObject page) = let
          Just (JsonArray threads) = Map.lookup "threads" page
          relevant = filter fiftyOrMore threads
          in map formMessage relevant

sendMessages :: [TelegramMessage] -> IO ()
sendMessages msgs = return ()  -- NOP
