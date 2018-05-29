{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
import Prelude
import Protolude hiding (putStrLn)
import Test.Hspec
import Test.QuickCheck
import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Test.DocTest
--------------------------------------------------------------------------------
default (Text)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  putStrLn "\nRunning Doc Tests!"
  doctest ["src/", "-XOverloadedStrings"]
  return ()
--------------------------------------------------------------------------------
