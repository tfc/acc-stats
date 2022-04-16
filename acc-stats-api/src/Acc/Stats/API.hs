{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module Acc.Stats.API where

import           Acc.StatsPage
import           Control.Lens.TH
import           Data.Aeson
import qualified Data.Map.Strict as HM
import           Data.Proxy
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import           Servant.API

data DataPoint = DataPoint
    { _dataGraphics :: GraphicsPage
    , _dataStat     :: StatPage
    , _dataPhysics  :: PhysicsPage
    } deriving (Eq, Generic, Show)

$(makeLenses ''DataPoint)

dataPointJsonOptions :: Options
dataPointJsonOptions = let l = length ("_data" :: String)
  in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance FromJSON DataPoint where
  parseJSON = genericParseJSON dataPointJsonOptions
instance ToJSON DataPoint where
  toJSON = genericToJSON dataPointJsonOptions

data DisplayData = DisplayData
    { displayPressures :: [Float]
    , displayFuel :: Float
    , displayAbs :: Int
    , displayBrakeBias :: Float
    , displayTc :: (Int, Int)
    , displayTrackStatus :: Text
    , displayTempAirRoad :: (Float, Float)
    , displayRainNowTenThirtyMin :: (Int, Int, Int)
    , displayTimeLast :: Text
    , displayTimeBest :: Text
    , displayTimeEstimated :: Text
    , displayTimeCurrent :: Text
    , displayFuelEstimatedLaps :: Float
    , displayFuelXLap :: Float
    , displayMfdPressures :: [Float]
    , displayMfdTyreSet :: Int
    , displayMfdFuelAdd :: Float
    } deriving (Eq, Generic, Show)

instance ToJSON DisplayData
instance FromJSON DisplayData

type AccStatsApi =
           Summary "List latest telemetry"
        :> "latest"
        :> Get '[JSON] (HM.Map Text DataPoint)
    :<|>   Summary "Post new data point"
        :> "post"
        :> ReqBody '[JSON] DataPoint
        :> Post '[JSON] ()
    :<|> "display" :> Get '[JSON] (HM.Map Text DisplayData)

accStatsApiProxy :: Proxy AccStatsApi
accStatsApiProxy = Proxy
