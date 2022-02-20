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
    describe "discordhook test" $ do
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
        
    describe "slackhook test" $ do
        describe "sample send" $ do
            it "send a is success" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook "a" . fromMaybe "")
                `shouldReturn` "ok"

        describe "send long long msg" $ do
            it "ok in slack" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook (S8.replicate 2001 'a') . fromMaybe "")
                `shouldReturn` "ok"

        describe "send long long msg 2" $ do
            it "ok in slack" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook (S8.replicate 4001 'a') . fromMaybe "")
                `shouldReturn` "ok"

        describe "send long long msg 3" $ do
            it "ok in slack" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook (S8.replicate 6001 'a') . fromMaybe "")
                `shouldReturn` "ok"

        describe "send over 10,000 msg" $ do
            it "ok in slack" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook (S8.replicate 10001 'a') . fromMaybe "")
                `shouldReturn` "ok"

        describe "send empty msg" $ do
            it "no test erorr" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook "" . fromMaybe "")
                `shouldReturn` "no_text"

        describe "send japanese row bytestring" $ do
            it "often return error literal" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook "こんにちは世界" . fromMaybe "")
                `shouldReturn` "invalid_payload"

        describe "send encodeUtf8 japanese" $ do
            it "send success !" $
                (lookupEnv "SLACKHOOK"
                >>= flip slackHook (S8.fromStrict $ T.encodeUtf8 "こんにちは世界") . fromMaybe "")
                `shouldReturn` "ok"