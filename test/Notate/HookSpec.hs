{-# LANGUAGE OverloadedStrings  #-}
module Notate.HookSpec where

import Test.Hspec

import Notate.Hook (discordHook, slackHook)
import System.Environment
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T 
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as S8

main :: IO ()
main = hspec spec 

spec :: Spec
spec = do
  describe "Start Hook module test" $ do
    describe "sample test" $ do
      it "add one to one is two" $
        1 + 1 `shouldBe` 2
    
    describe "sample send" $ do
      it "send a is success" $
        (lookupEnv "DISCORDHOOK"
        >>= flip discordHook "a" . fromMaybe "")
         `shouldReturn` ""

    describe "send long long msg" $ do
      it "return error literal" $
        (lookupEnv "DISCORDHOOK"
        >>= flip discordHook (S8.replicate 2001 'a') . fromMaybe "")
         `shouldReturn` "{\"content\": [\"Must be 2000 or fewer in length.\"]}"

    describe "send empty msg" $ do
      it "return error literal" $
        (lookupEnv "DISCORDHOOK"
        >>= flip discordHook "" . fromMaybe "")
         `shouldReturn` "{\"message\": \"Cannot send an empty message\", \"code\": 50006}"

    describe "send japanese row bytestring" $ do
      it "often return error literal" $
        (lookupEnv "DISCORDHOOK"
        >>= flip discordHook "こんにちは世界" . fromMaybe "")
         `shouldReturn` "{\"message\": \"Cannot send an empty message\", \"code\": 50006}"

    describe "send encodeUtf8 japanese" $ do
      it "send success !" $
        (lookupEnv "DISCORDHOOK"
        >>= flip discordHook (S8.fromStrict $ T.encodeUtf8 "こんにちは世界") . fromMaybe "")
         `shouldReturn` ""