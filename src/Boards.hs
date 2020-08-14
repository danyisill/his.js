{-# LANGUAGE OverloadedStrings #-}

module Boards where

import JSON
import EscapeHTML

import Network.HTTP.Simple
import qualified Data.ByteString.Lazy.Char8 as L8

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

  let jsonString = L8.unpack $ getResponseBody res
  let json = parseJSON jsonString
  putStrLn $ show json

  return [TelegramMessage "a" "b" "c"]
  where catalog :: String
        catalog = "http://a.4cdn.org/" ++ board ++ "/catalog.json"

sendMessages :: [TelegramMessage] -> IO ()
sendMessages msgs = return ()
