{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Acc.Session.State
import           Acc.StatsPage
import           AccMapping
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (void)
import           Control.Monad.State.Strict
import           System.IO.Error            (catchIOError)

telemetryUser :: IO FullData -> IO ()
telemetryUser readData = void $ execStateT f freshSession
    where f = forever $ do
            fd@(FullData pg pp ps) <- liftIO readData
            when (isRunningSession fd) $ do
                updateStintState pp pg >>= \case
                    Nothing -> return ()
                    Just s -> do
                        liftIO $ print s
                        s <- get
                        liftIO $ print $ _dataPoints $ _currentStint s

            liftIO $ threadDelay 100000

isRunningSession :: FullData -> Bool
isRunningSession (FullData pg _ ps) =
    _statPageAcVersion ps /= "" && _graphicsPageStatus pg == 2

main :: IO ()
main = f
    where
        retryF = putStrLn "acc is not running" >> threadDelay 3000000 >> f
        f = withMappings telemetryUser `catchIOError` const retryF
