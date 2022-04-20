{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Acc.Session.State
import           Acc.StatsPage
import           AccMapping
import           Control.Concurrent (threadDelay)
import           System.IO.Error    (catchIOError)
import           Control.Monad.State.Strict

telemetryUser :: IO FullData -> IO ()
telemetryUser readData = execStateT f freshSession >> return ()
    where f = forever $ do
            FullData pg pp ps <- liftIO $ readData
            if _statPageAcVersion ps == ""
                then return ()
                else if _graphicsPageStatus pg == 3
                    then return ()
                    else do
                        updateStintState pp pg >>= \case
                            Nothing -> return ()
                            Just s -> liftIO $ print s
            liftIO $ threadDelay 100000

main :: IO ()
main = f
    where
        retryF = putStrLn "acc is not running" >> threadDelay 3000000 >> f
        f = withMappings telemetryUser `catchIOError` \_ -> retryF
