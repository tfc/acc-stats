{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.DataStructures where

import           Control.Lens.TH
import           Flat
import           GHC.Generics

data Lap = Lap
    { _sectorTimes :: ![Int]
    , _lapTime     :: !Int
    , _lapValid    :: !Bool
    , _inLap       :: !Bool
    , _outLap      :: !Bool
    } deriving (Eq, Flat, Generic, Show)

makeLenses ''Lap

data LapTelemetry = LapTelemetry
    { _telNormPosition :: !Float
    , _telSpeed        :: !Float
    , _telGas          :: !Float
    , _telBrake        :: !Float
    } deriving (Flat, Generic, Show)

makeLenses ''LapTelemetry

data SessionEvent = FinishedSector Int
                  | FinishedLap Lap [LapTelemetry]
                  | FinishedStint Float
                  deriving (Flat, Generic, Show)
