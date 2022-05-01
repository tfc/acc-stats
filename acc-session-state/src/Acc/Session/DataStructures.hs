{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.DataStructures where

import           Control.Lens.TH
import           GHC.Generics

data Lap = Lap
    { _sectorTimes :: ![Int]
    , _lapTime     :: !Int
    , _lapValid    :: !Bool
    , _inLap       :: !Bool
    , _outLap      :: !Bool
    } deriving (Eq, Generic, Show)

makeLenses ''Lap

data LapTelemetry = LapTelemetry
    { _telNormPosition :: !Float
    , _telSpeed        :: !Float
    , _telGas          :: !Float
    , _telBrake        :: !Float
    } deriving (Show)

makeLenses ''LapTelemetry
