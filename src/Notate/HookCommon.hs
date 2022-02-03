{-# LANGUAGE OverloadedStrings  #-}
module Notate.HookCommon where

import Data.Aeson (Value)
import qualified Data.ByteString.Char8      as BS
import qualified Data.ByteString.Lazy.Char8 as S8
import qualified Data.String                as S
import           Network.HTTP.Simple

postJson :: S.String -> Value -> IO BS.ByteString 
postJson url json = do

  request' <- parseRequest url
  let request
        = setRequestMethod "POST"
        $ setRequestSecure True
        $ setRequestPort 443
        $ setRequestBodyJSON json
        $ request'
  getResponseBody <$> httpBS request