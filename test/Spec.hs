{-#
LANGUAGE
  DataKinds,
  DeriveAnyClass,
  DeriveGeneric,
  ConstraintKinds,
  ExtendedDefaultRules,
  FlexibleContexts,
  FlexibleInstances,
  TypeFamilies,
  TypeOperators,
  OverloadedStrings,
  OverloadedLabels
#-}
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
import SuperRecord
--------------------------------------------------------------------------------
import Database
import Scheduler
import Types
--------------------------------------------------------------------------------
default (Text)
--------------------------------------------------------------------------------
main :: IO ()
main = do
  doctest ["src/", "-XOverloadedStrings"]
  hspec spec
  return ()
--------------------------------------------------------------------------------
spec :: Spec
spec = do
 describe "jobs" $ do
   it "registers a job" $ do
     let jobForm = object [ "title"      .= "List Files"
                          , "code"       .= "ls"
                          , "frequency"  .= "1 minute"
                          ]
     jobs <- Database.post "/jobs" jobForm :: IO [Rec Job]
     let Just job = head jobs
     job &. #title      `shouldBe` "List Files"
     job &. #code       `shouldBe` "ls"
     job &. #frequency  `shouldBe` "00:01:00"
--------------------------------------------------------------------------------
