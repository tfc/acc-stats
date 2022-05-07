module Main where

import           Acc.Session.DataStructures
import           Acc.Stats.API
import           Acc.Stats.Client
import qualified Acc.StatsPage.Examples     as Ex
import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forever, forM_)
import           Flat
import           Network.HTTP.Client.TLS    (newTlsManager)
import           Servant.Client
import qualified Data.ByteString as BS

main :: IO ()
main = let
        lap = Lap [10, 20, 30] 60 True False False
        te = LapTelemetry 0 0 0 0
        se = FinishedLap lap $ take 2000 $ repeat te
    in do
        baseUrl <- parseBaseUrl "http://localhost:8000"
        putStrLn $ showBaseUrl baseUrl
        httpClientManager <- newTlsManager

        let routes = accSessionRoutes httpClientManager baseUrl
            postNewSession = _postNewSession routes
            putNewEvent = _putNewEvent routes

        forever $ do
            sessionId <- postNewSession
            putStrLn $ "Got Session ID " <> show sessionId
            forM_ [0..10] $ \_ -> do
                _ <- putNewEvent sessionId se
                putStrLn "Sent LapEvent"
                print $ BS.length $ flat se
                threadDelay 3000000
            putNewEvent sessionId $ FinishedStint 10.0
