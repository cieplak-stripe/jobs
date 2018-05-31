{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Database
  ( getResponseBody
  , get
  , post
  ) where
--------------------------------------------------------------------------------
import Data.Aeson
import Network.HTTP.Client
import Network.HTTP.Simple
import Protolude hiding (get)
--------------------------------------------------------------------------------
-- postgREST config hard-coded for now

host' = "localhost"
port' = 7080
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
post :: FromJSON a => Text -> Value -> IO a -- IO (Either Response) a
post path json = do
  response <- post' host' port' (encodeUtf8 path) (encode json)
  let maybeDecoded = decode (getResponseBody response) :: (FromJSON a) => Maybe a
  case maybeDecoded of
    Just decoded -> return decoded -- (Right decoded)
    -- Nothing      -> return (Left response)
--------------------------------------------------------------------------------
get ::  FromJSON a => Text -> IO a -- (Either Response) a
get path = do
  response <- get' host' port' (encodeUtf8 path)
  let maybeDecoded = decode (getResponseBody response) :: (FromJSON a) => Maybe a
  case maybeDecoded of
    Just decoded -> return decoded --(Right decoded)
--    Nothing      -> return (Left response)
--------------------------------------------------------------------------------
