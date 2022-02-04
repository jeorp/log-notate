{-# LANGUAGE OverloadedStrings  #-}
module NotateLogger where

import Control.Monad (void, unless)
import Control.Monad.Logger
import Control.Monad.IO.Class (MonadIO)

import Data.List 
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
          loop l 
           | null l = return ()
           | snd (head l) >= 2000 = B.putStr (fst (head l) <> "\n is skiped\n") >> loop (tail l)
           | otherwise = let len_index = foldl (\a b -> let l = fst a + snd b + 1 in if l <= 2000 then (l, snd a + 1) else a) (0, 1) l
                             ls = splitAt (snd len_index) l
                         in void (f url $ S8.fromStrict $ B.intercalate "\r" logs) >> loop (snd ls)
      in loop $ zip logs (map B.length logs)

runDiscordHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runDiscordHookLoggingT = runHookLoggingT discordHook

runSlackHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runSlackHookLoggingT = runHookLoggingT slackHook


