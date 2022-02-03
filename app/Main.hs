{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings  #-}
module Main where

import Control.Monad.Logger
import NotateLogger (runDiscordHookLoggingT)

main :: IO ()
main = runDiscordHookLoggingT "https://discordapp.com/api/webhooks/XXXXXXX/YYYYYYYY" $ do
  $(logInfo) "aaaa"
  $(logInfo) "bbbb"
