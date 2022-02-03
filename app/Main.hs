{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import qualified Data.Text as T

import Control.Monad.Logger
import NotateLogger (runDiscordHookLoggingT)


main :: IO ()
main = runDiscordHookLoggingT "https://discordapp.com/api/webhooks/XXXXXXXXXXXXX/YYYYYYYYYYYY" $ do
  $(logInfo) $ T.pack $ take 1960 $ repeat 't' -- note : over 2000 lengh ByteString is not send at once, so one of too long log is skipped.
  $(logInfo) "aaaa"                            --        in detail please see the api reference and my implement.
  $(logInfo) "bbbb"

