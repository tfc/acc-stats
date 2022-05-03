{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}
module Acc.Stats.API where

import           Acc.Session.DataStructures
import           Data.Aeson
import           Data.Bifunctor             (bimap)
import           Data.ByteString.Lazy       (fromStrict)
import           Data.Proxy
import           Data.Text                  (Text)
import           Flat
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API
import           Servant.API.Generic

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


displayDataJsonOptions :: Options
displayDataJsonOptions = let l = length ("display" :: String)
  in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance ToJSON DisplayData where
  toJSON = genericToJSON displayDataJsonOptions
instance FromJSON DisplayData where
  parseJSON = genericParseJSON displayDataJsonOptions

data FlatContentType

instance Accept FlatContentType where
   contentType _ = "acc-stats-flat" // "acc.stats" /: ("charset", "utf-8")

instance Flat a => MimeRender FlatContentType a where
    mimeRender _ = fromStrict . flat

instance Flat a => MimeUnrender FlatContentType a where
    mimeUnrender _ = bimap show id . unflat

data SessionRoutes route = SessionRoutes
    { _postNewSession :: route :- Post '[JSON] Int
    , _putNewEvent :: route :- Capture "id" Int
                            :> ReqBody '[FlatContentType] SessionEvent
                            :> Put '[JSON] ()
    } deriving (Generic)

accStatsApiProxy :: Proxy (ToServantApi SessionRoutes)
accStatsApiProxy = genericApi (Proxy :: Proxy SessionRoutes)
