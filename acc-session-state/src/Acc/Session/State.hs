{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.State where

import           Acc.StatsPage
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.State.Strict
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
    , _stintDistance = 0.0
    }

freshSession = Session
    { _stints = []
    , _currentStint = freshStint
    }

finalizeLap :: Int -> Lap -> Lap
finalizeLap lastTime lap = let
    sectors = lap ^. sectorTimes ++ [0]
    sectorDiffs = reverse $ zipWith (-) (lastTime:sectors) sectors
    in
        lap & lapTime .~ lastTime
            & sectorTimes .~ sectorDiffs

isLegitLap :: Lap -> Bool
isLegitLap lap = length (lap ^. sectorTimes) == 3


startFreshLap :: Stint -> Stint
startFreshLap = (& currentLap .~ freshLap)
              . (& sector .~ 0)

appendNewLapToOldLaps :: Lap -> Stint -> Stint
appendNewLapToOldLaps newLap = finishedLaps %~ (newLap:)

updateLapState :: Monad m
               => GraphicsPage
               -> StateT Stint m (Maybe LapEvent)
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

    let lastSector = s ^. sector
    modify $ sector .~ currentSector

    -- normal sector advance = same lap
    if currentSector > lastSector
        then do
                modify $ currentLap . sectorTimes %~ (lastSectorTime:)
                return $ Just $ FinishedSector lastSector
        else
            -- new lap
            if currentSector < lastSector
               then do
                   let newLap = finalizeLap lastTime (s ^. currentLap)
                   modify startFreshLap

                   if isLegitLap newLap
                       then modify (appendNewLapToOldLaps newLap)
                         >> return (Just $ FinishedLap newLap)
                       else return Nothing
                else return Nothing


storeStint :: Session -> Session
storeStint s = freshSession { _stints = s ^. currentStint : s ^. stints }

updateStintState :: Monad m
                 => PhysicsPage
                 -> GraphicsPage
                 -> StateT Session m (Maybe SessionEvent)
updateStintState pp gp = let
        distanceTraveled = gp ^. graphicsPageDistanceTraveled
    in do
        s <- get
        let lastDistanceTraveled = s ^. currentStint . stintDistance

        modify $ currentStint . stintDistance .~ distanceTraveled

        -- new stint
        if distanceTraveled < lastDistanceTraveled
           then do
               modify storeStint
               return $ Just NewStintEvent
            else zoom currentStint $ updateLapState gp >>= \case
                Nothing -> return Nothing
                Just s -> return $ Just $ LapEv s



data LapEvent = FinishedSector Int
              | FinishedLap Lap
              deriving Show

data SessionEvent = NewStintEvent
                  | LapEv LapEvent
                  deriving Show
