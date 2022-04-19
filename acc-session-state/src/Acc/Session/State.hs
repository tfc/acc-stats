{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.State where

import           Acc.StatsPage
import Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.State.Strict
import           GHC.Generics

data Lap = Lap
    { _sectorTimes :: [Int]
    , _lapTime        :: Int
    , _lapValid       :: Bool
    , _inLap       :: Bool
    , _outLap      :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''Lap

data Stint = Stint
    { _sector      :: Int
    , _currentLap :: Lap
    , _finishedLaps :: [Lap]
    } deriving (Generic, Show)

makeLenses ''Stint

freshLap = Lap
    { _sectorTimes = []
    , _lapTime = 0
    , _lapValid = True
    , _inLap = False
    , _outLap = False
    }

freshStint = Stint
    { _sector = 2 -- end of imaginary lap
    , _currentLap = freshLap
    , _finishedLaps = []
    }

storeLap :: Int -> Stint -> Stint
storeLap lastTime s =
    if length (s ^. currentLap . sectorTimes) /= 2
       then newLapAdded s
       else oldLapStored $ newLapAdded s
    where
      sectors = (lastTime : (s ^. currentLap . sectorTimes)) ++ [0]
      sectorDiffs = reverse $ zipWith (-) sectors (tail sectors)
      newLap = (s ^. currentLap)
        & lapTime .~ lastTime
        & sectorTimes .~ sectorDiffs

      newLapAdded = (& currentLap .~ freshLap)
                  . (& sector .~ 0)
      oldLapStored = finishedLaps %~ (newLap:)

updateLapState :: Monad m => GraphicsPage -> StateT Stint m ()
updateLapState gp = let
        isInPitLane = gp ^. graphicsPageIsInPitLane /= 0
        currentSector = gp ^. graphicsPageCurrentSectorIndex
        lastSectorTime = gp ^. graphicsPageLastSectorTime
        lastTime = gp ^. graphicsPageILastTimeMs
        currentLapValid = gp ^. graphicsPageIsValidLap /= 0
    in do
    s <- get

    modify $ (& currentLap . outLap %~ (|| (isInPitLane && currentSector == 0)))
           . (& currentLap . inLap  %~ (|| (isInPitLane && currentSector == 2)))
           . (& currentLap . lapValid %~ (&& currentLapValid))

    -- normal sector advance = same lap
    when (currentSector > s ^. sector) $ modify
        $ (& currentLap . sectorTimes %~ (lastSectorTime:))

    -- new lap
    when (currentSector < s ^. sector) $ modify $ storeLap lastTime

    modify (sector .~ currentSector)
