{-# LANGUAGE OverloadedStrings  #-}
module Notate.Hook (discordHook, slackHook) where

import Data.Maybe (fromMaybe)
import Data.Aeson (decode, Value(Null))
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.String                as S
import Notate.HookCommon (postJson)

discordHook :: String -> S8.ByteString  -> IO BS.ByteString 
discordHook url msg =
    let t = decode ("{\"content\": \"" <> msg <> "\"}") :: Maybe Value
        json = fromMaybe Null t
        in postJson url json

slackHook :: String -> S8.ByteString  -> IO BS.ByteString 
slackHook url msg =
    let t = decode ("{\"text\": \"" <> msg <> "\"}") :: Maybe Value
        json = fromMaybe Null t
        in postJson url json