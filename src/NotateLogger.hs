{-# LANGUAGE OverloadedStrings  #-}
module NotateLogger where

import Control.Monad (void, unless)
import Control.Monad.Logger
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Cont

import Data.List 
import Data.Foldable (foldlM)
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
           | otherwise = let len_index = len_indexC l
                             ls = splitAt (snd len_index) l
                         in void (f url $ S8.fromStrict $ B.intercalate "\r" logs) >> loop (snd ls)
      in loop $ zip logs (map B.length logs)

runDiscordHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runDiscordHookLoggingT = runHookLoggingT discordHook

runSlackHookLoggingT :: MonadIO m => String -> LoggingT m a -> m a
runSlackHookLoggingT = runHookLoggingT slackHook

foldlC :: (b -> a -> b) -> (b -> Bool) -> b -> [a] -> b
foldlC cf pred b (x:xs) = 
  let cal = cf b x
  in if pred cal
    then foldlC cf pred cal xs 
    else cal 
foldlC cf pred b [] = b 

foldlC' :: (b -> a -> Cont b b) -> b -> [a] -> Cont b b
foldlC' = foldlM

len_index :: [(B.ByteString , Int)] -> (Int , Int)
len_index = foldlC (\b a -> let l = fst b + snd a + 1 in (l, snd b + 1)) ((<= 2000) . fst) (0, 1)

len_indexC :: [(B.ByteString , Int)] -> (Int , Int)
len_indexC = flip runCont id . lenIndexC
  where
    lenIndexC :: [(B.ByteString , Int)] -> Cont (Int, Int) (Int, Int)
    lenIndexC = foldlC' 
      (\b a -> 
        callCC $ \exit -> do let l = fst b + snd a + 1 in if l <= 2000 then return (l, snd b + 1) else exit b
      )
      (0, 1)

