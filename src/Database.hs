{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Database
  ( getResponseBody
  , get
  , post
  ) where
--------------------------------------------------------------------------------
import Data.Aeson
--import Data.ByteString.Char8 (decodeUtf8, encodeUtf8)
import Network.HTTP.Client
import Network.HTTP.Simple
import Prelude ((!!))
import Protolude hiding (get)
--------------------------------------------------------------------------------
post' :: ByteString
     -> Int
     -> ByteString
     -> LByteString
     -> IO (Response LByteString)
post' host port path body = do
  let request = setRequestMethod    "POST"
                $ setRequestHost    host
                $ setRequestPort    port
                $ setRequestPath    path
                $ setRequestBodyLBS body
                $ setRequestHeader  "Prefer"       ["return=representation"]
                $ setRequestHeader  "Content-Type" ["application/json"]
                $ setRequestHeader  "Accept"       ["application/json"]
                $ defaultRequest
  response <- httpLBS request
  return response
--------------------------------------------------------------------------------
get'  :: ByteString
      -> Int
      -> ByteString
      -> IO (Response LByteString)
get' host port path = do
  let request = setRequestMethod    "GET"
                $ setRequestHost    host
                $ setRequestPort    port
                $ setRequestPath    path
                $ setRequestHeader  "Accept"       ["application/json"]
                $ defaultRequest
  response <- httpLBS request
  return response
--------------------------------------------------------------------------------
post :: FromJSON a => Text -> Value -> IO a
post path json = do
  response <- post' "localhost" 7080 (encodeUtf8 path) (encode json)
  let maybeDecoded = decode (getResponseBody response) :: (FromJSON a) => Maybe [a]
  case maybeDecoded of
    Just decoded -> return (decoded !! 0) -- TODO
    -- TODO
--------------------------------------------------------------------------------
get ::  FromJSON a => Text -> IO [a]
get path = do
  response <- get' "localhost" 7080 (encodeUtf8 path)
  let maybeDecoded = decode (getResponseBody response) :: (FromJSON a) => Maybe [a]
  case maybeDecoded of
    Just decoded -> return decoded
    Nothing      -> return [] -- TODO
--------------------------------------------------------------------------------
