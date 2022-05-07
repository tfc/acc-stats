{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Acc.Session.State
import           Acc.Session.DataStructures
import           Acc.Stats.API
import           Acc.Stats.Client
import           Acc.StatsPage
import           AccMapping
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (void)
import           Control.Monad.State.Strict
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Servant.Client
import           System.IO.Error            (catchIOError)
import Data.IORef

telemetryUser :: IO FullData -> IO ()
telemetryUser readData = do
        s <- initState

        baseUrl <- parseBaseUrl "http://192.168.178.86:8000"
        putStrLn $ showBaseUrl baseUrl
        httpClientManager <- newTlsManager
        let routes = accSessionRoutes httpClientManager baseUrl
        void $ execStateT (f routes) s
    where
      initState = do
            (FullData ipg _ _) <- liftIO readData
            return $ freshStintState ipg ipg
      f = \routes -> do
        let postNewSession = _postNewSession routes
            putNewEvent = _putNewEvent routes
        sessionIdVar <- liftIO (newIORef Nothing :: IO (IORef (Maybe Int)))
        forever $ do
            fd@(FullData pg pp _) <- liftIO readData

            when (isRunningSession fd) $ do
                liftIO $ do
                    sid' <- readIORef sessionIdVar
                    when (Nothing == sid') $ do
                        putStrLn "Requesting new session id..."
                        newId <- postNewSession
                        writeIORef sessionIdVar $ Just newId
                        putStrLn $ "new id is" <> show newId
                Just sid <- liftIO $ readIORef sessionIdVar
                events <- updateStint pg pp
                forM_ events $ \case
                    FinishedSector n -> liftIO $ putStrLn $ "finished sector " <> show n
                    fle@(FinishedLap _ _) -> liftIO $ do
                        putStrLn "posting new lap"
                        putNewEvent sid fle
                    FinishedStint _ -> liftIO $ do
                        putStrLn "finished stint"
                        writeIORef sessionIdVar Nothing
                    x -> liftIO $ print x

            liftIO $ threadDelay 1000000

isRunningSession :: FullData -> Bool
isRunningSession (FullData pg _ ps) =
    _statPageAcVersion ps /= "" && _graphicsPageStatus pg == 2

main :: IO ()
main = do
    f
    where
        retryF = putStrLn "acc is not running" >> threadDelay 3000000 >> f
        f = withMappings telemetryUser `catchIOError` const retryF
