{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE FlexibleContexts #-}
--------------------------------------------------------------------------------
import Test.DocTest
import Test.Hspec
import Test.QuickCheck
--------------------------------------------------------------------------------
import Control.Concurrent (forkIO)
import Control.Exception (evaluate)
import Control.Lens hiding ((.=))
import Data.Aeson
import Prelude   hiding (head)
import Protolude hiding (putStrLn)
--------------------------------------------------------------------------------
import Database
import Scheduler
import Types
--------------------------------------------------------------------------------
default (Text)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  -- putStrLn "\nRunning Doc Tests!"
  -- doctest ["src/", "-XOverloadedStrings"]
  hspec spec
  return ()
--------------------------------------------------------------------------------
spec :: Spec
spec = do
 describe "jobs" $ do
   it "registers a job" $ do
     let jobForm = object [ "title"      .= "List Files"
                          , "code"       .= "ls"
                          , "start_time" .= "2018-05-29"
                          , "frequency"  .= "1 hour"
                          ]
     jobs <- Database.post "/jobs" jobForm :: IO [Job]
     let Just job = head jobs
     job ^. _title      `shouldBe` "List Files"
     job ^. _code       `shouldBe` "ls"
     job ^. _start_time `shouldBe` "2018-05-29T00:00:00"
     job ^. _frequency  `shouldBe` "01:00:00"
--------------------------------------------------------------------------------
