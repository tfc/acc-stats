{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Acc.Stats.Server where

import           Acc.Stats.API
import           Acc.StatsPage
import           Control.Concurrent.MVar
import           Control.Monad           (when)
import           Control.Monad.IO.Class  (liftIO)
import qualified Data.Map.Strict         as HM
import           Data.Text               (Text)
import qualified Data.Vector.Storable    as V
import           Servant

type TC = MVar (HM.Map Text DataPoint)

emptyServerState :: IO TC
emptyServerState = newMVar HM.empty

getTelemetryCollection :: TC -> Handler (HM.Map Text DataPoint)
getTelemetryCollection = liftIO . readMVar

pushDataPoint :: TC -> DataPoint -> Handler ()
pushDataPoint mtc dp =
    let
        name = _statPagePlayerName (_dataStat dp)
            <> " "
            <> _statPagePlayerSurname (_dataStat dp)
            <> " "
            <> _statPageNick (_dataStat dp)
        pushIt = liftIO $ modifyMVar_ mtc $ return . HM.insert name dp
        dataIsUseful = name /= ""
    in when dataIsUseful pushIt

getPressures :: TC -> Handler (HM.Map Text DisplayData)
getPressures mtc = let
    g :: DataPoint -> DisplayData
    g dp = let
            pp = _dataPhysics dp
            gp = _dataGraphics dp
        in DisplayData
            { displayPressures = (V.toList $ _physicsPageWheelsPressure pp)
            , displayFuel = _physicsPageFuel pp
            , displayAbs = _graphicsPageABS gp
            , displayTc = (_graphicsPageTC gp, _graphicsPageTCCut gp)
            , displayBrakeBias = _physicsPageBrakeBias pp
            , displayTrackStatus = _graphicsPageTrackStatus gp
            , displayTempAirRoad = (_physicsPageAirTemp pp, _physicsPageRoadTemp pp)
            , displayRainNowTenThirtyMin =
                ( _graphicsPageRainIntensity gp
                , _graphicsPageRainIntensityIn10min gp
                , _graphicsPageRainIntensityIn30min gp
                )
            , displayTimeLast = _graphicsPageLastTime gp
            , displayTimeBest = _graphicsPageBestTime gp
            , displayTimeEstimated = _graphicsPageEstimatedLapTime gp
            , displayTimeCurrent = _graphicsPageCurrentTime gp
            , displayFuelEstimatedLaps = _graphicsPageFuelEstimatedLaps gp
            , displayFuelXLap = _graphicsPageFuelXLap gp
            , displayMfdPressures =
                [ _graphicsPageMfdTyrePressureLF gp
                , _graphicsPageMfdTyrePressureRF gp
                , _graphicsPageMfdTyrePressureLR gp
                , _graphicsPageMfdTyrePressureRR gp
                ]
            , displayMfdTyreSet = _graphicsPageMfdTyreSet gp
            , displayMfdFuelAdd = _graphicsPageMfdFuelToAdd gp
            }
    in do
        tc <- liftIO (readMVar mtc)
        return $ HM.map g tc

server :: TC -> Server AccStatsApi
server tc = getTelemetryCollection tc
       :<|> pushDataPoint tc
       :<|> getPressures tc
