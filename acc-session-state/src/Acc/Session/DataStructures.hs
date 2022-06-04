{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.DataStructures where

import           Control.Lens.TH
import           Data.Time
import           Flat
import           GHC.Generics
import           Timestamp

instance Flat Timestamp

data Lap = Lap
    { _sectorTimes :: ![Int]
    , _lapTime     :: !Int
    , _lapValid    :: !Bool
    , _inLap       :: !Bool
    , _outLap      :: !Bool
    } deriving (Eq, Flat, Generic, Show)

makeLenses ''Lap

data LapTelemetry = LapTelemetry
    { _telTimestamp      :: Timestamp
    , _telNormPosition   :: !Float
    , _telGas            :: !Float
    , _telBrake          :: !Float
    , _telGear           :: !Int
    , _telRpms           :: !Int
    , _telSpeed          :: !Float
    , _telSteerAngle     :: !Float
    , _telWheelPressures :: ![Float]
    , _telWheelTemps     :: ![Float]
    } deriving (Flat, Generic, Show)

makeLenses ''LapTelemetry

data SessionEvent = FinishedSector Int
                  | FinishedLap Lap [LapTelemetry]
                  | FinishedStint Float
                  deriving (Flat, Generic, Show)
