{-# LANGUAGE OverloadedStrings #-}
--------------------------------------------------------------------------------
module Database
  ( getResponseBody
  , get
  , post
  ) where
--------------------------------------------------------------------------------
import Prelude ()
import Protolude hiding (get)
import Network.HTTP.Client
import Network.HTTP.Simple
--------------------------------------------------------------------------------
post :: ByteString
     -> Int
     -> ByteString
     -> LByteString
     -> IO (Response LByteString)
post host port path body = do
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
get  :: ByteString
     -> Int
     -> ByteString
     -> IO (Response LByteString)
get host port path = do
  let request = setRequestMethod    "GET"
                $ setRequestHost    host
                $ setRequestPort    port
                $ setRequestPath    path
                $ setRequestHeader  "Accept"       ["application/json"]
                $ defaultRequest
  response <- httpLBS request
  return response
--------------------------------------------------------------------------------
