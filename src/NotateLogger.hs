{-# LANGUAGE OverloadedStrings  #-}
module NotateLogger where

import Control.Monad (void)
import Control.Monad.Logger
import Control.Monad.IO.Class (MonadIO)

import qualified Data.Text as T 
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as S8

import Notate.Hook (discordHook, slackHook)

runHookLoggingT :: MonadIO m => (String -> S8.ByteString -> IO b) -> String -> LoggingT m a -> m a
runHookLoggingT f url =  (`runLoggingT` hook)
  where
    hook loc src level msg = 
      let logs =  (B.lines . fromLogStr) $ defaultLogStr loc src level msg
          len = sum $ map B.length logs  -- too long text is send failed. latter
          ls = B.intercalate "\r" logs
      in void $ f url $ S8.fromStrict ls

runDiscordHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runDiscordHookLoggingT = runHookLoggingT discordHook

runSlackHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runSlackHookLoggingT = runHookLoggingT slackHook


