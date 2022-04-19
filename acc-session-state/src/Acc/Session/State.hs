{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.State where

import Data.Maybe (fromJust)
import           Acc.StatsPage
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.State.Strict
import qualified Data.Vector.Storable       as V
import           GHC.Generics

data Lap = Lap
    { _sectorTimes :: [Int]
    , _lapTime     :: Int
    , _lapValid    :: Bool
    , _inLap       :: Bool
    , _outLap      :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''Lap

data Stint = Stint
    { _sector           :: Int
    , _currentLap       :: Lap
    , _finishedLaps     :: [Lap]
    , _initialPressures :: V.Vector Float
    , _maxPressures     :: V.Vector Float
    , _avgPressures     :: V.Vector Float
    , _dataPoints       :: Int
    , _stintDistance    :: Float
    } deriving (Generic, Show)

makeLenses ''Stint

data Session = Session
    { _stints          :: [Stint]
    , _currentStint    :: Stint
    } deriving (Generic, Show)

makeLenses ''Session

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
    , _initialPressures = V.empty
    , _maxPressures = V.fromList [0, 0, 0, 0]
    , _avgPressures = V.fromList [0, 0, 0, 0]
    , _dataPoints = 0
    , _stintDistance = 0.0
    }

freshSession = Session
    { _stints = []
    , _currentStint = freshStint
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

updateLapState :: Monad m
               => GraphicsPage
               -> StateT Stint m ()
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
    when (currentSector > s ^. sector) $ modify $
        currentLap . sectorTimes %~ (lastSectorTime:)

    -- new lap
    when (currentSector < s ^. sector) $ modify $ storeLap lastTime

    modify $ sector .~ currentSector

storeStint :: Session -> Session
storeStint s = freshSession { _stints = s ^. currentStint : s ^. stints }

cumulativeAverage :: Int -> Float -> Float -> Float
cumulativeAverage n x oldAvg = (x + fromIntegral n * oldAvg) / (fromIntegral n + 1)

updateStintState :: Monad m
                 => PhysicsPage
                 -> GraphicsPage
                 -> StateT Session m ()
updateStintState pp gp = let
        distanceTraveled = gp ^. graphicsPageDistanceTraveled
        pressures = pp ^. physicsPageWheelsPressure
        tyreTemps = pp ^. physicsPageTyreCoreTemperature
        validPhysicsData = tyreTemps V.! 0 > 0.1
    in do
        s <- get
        let currentDataPoints = s ^. currentStint . dataPoints

        zoom currentStint $ updateLapState gp

        -- new stint
        when (distanceTraveled < s ^. currentStint . stintDistance) $ modify storeStint

        when validPhysicsData $ do
            when (s ^. currentStint . initialPressures == V.empty) $
                modify $ currentStint . initialPressures .~ pressures

            modify $ (& currentStint . maxPressures %~ V.zipWith max pressures)
                   . (& currentStint . avgPressures %~ V.zipWith (cumulativeAverage currentDataPoints) pressures)
                   . (& currentStint . dataPoints %~ (+1))


        modify $ currentStint . stintDistance .~ distanceTraveled
