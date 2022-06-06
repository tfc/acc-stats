{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import           Acc.Session.DataStructures
import           Acc.Session.State
import           Acc.Stats.API
import           Acc.Stats.Client
import           Acc.StatsPage
import           AccMapping
import           Control.Concurrent         (threadDelay)
import           Control.Monad.State.Strict
import qualified Data.ByteString            as BS
import           Data.IORef
import           Data.Maybe
import           Flat
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Servant.Client             hiding (baseUrl)
import           System.IO.Error            (catchIOError)

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
            fd@(FullData pg pp ps) <- liftIO readData

            when (isRunningSession fd) $ do
                liftIO $ do
                    sid' <- readIORef sessionIdVar
                    when (isNothing sid') $ do
                        putStrLn "Requesting new session id..."
                        newId <- postNewSession $ StintInfo (_statPageTrack ps) (_statPageCarModel ps)
                        writeIORef sessionIdVar $ Just newId
                        putStrLn $ "new id is" <> show newId
                Just sid <- liftIO $ readIORef sessionIdVar
                events <- updateStint pg pp
                forM_ events $ \case
                    FinishedSector n -> liftIO $ putStrLn $ "finished sector " <> show n
                    fle@(FinishedLap _ te) -> liftIO $ do
                        putStrLn $ "posting new lap, " <> show (length te) <> " telemetry points take " <> show (BS.length $ flat te) <> " bytes"
                        putNewEvent sid fle
                    FinishedStint _ -> liftIO $ do
                        putStrLn "finished stint"
                        writeIORef sessionIdVar Nothing
                    x -> liftIO $ print x

            liftIO $ threadDelay (10^6 `div` 1000 :: Int) -- ~100 data points per second

isRunningSession :: FullData -> Bool
isRunningSession (FullData pg _ ps) =
    _statPageAcVersion ps /= "" && _graphicsPageStatus pg == 2

main :: IO ()
main = do
    f
    where
        retryF = putStrLn "acc is not running" >> threadDelay 3000000 >> f
        f = withMappings telemetryUser `catchIOError` const retryF
