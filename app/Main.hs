{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import qualified Data.Text as T

import Control.Monad.Logger
import NotateLogger (runDiscordHookLoggingT)


main :: IO ()
main = runDiscordHookLoggingT "https://discordapp.com/api/webhooks/XXXXXXXX/YYYYYYYYYY" $ do
  $(logInfo) "あ"
  $(logInfo) $ T.replicate 1960 "t" -- note : over 2000 length ByteString is not send at once, so a too long log is skipped.
  $(logInfo) "aaaa"                            --        in detail please see the api reference and my implement.


