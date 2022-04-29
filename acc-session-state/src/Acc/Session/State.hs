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

data StintState = StintState
    { _sector           :: Int
    , _currentLap       :: Lap
    , _stintDistance    :: Float
    } deriving (Generic, Show)

makeLenses ''StintState

data Session = Session
    { _currentStint    :: StintState
    } deriving (Generic, Show)

makeLenses ''Session

freshLap = Lap
    { _sectorTimes = []
    , _lapTime = 0
    , _lapValid = True
    , _inLap = False
    , _outLap = False
    }

freshStint = StintState
    { _sector = 2 -- end of imaginary lap
    , _currentLap = freshLap
    , _stintDistance = 0.0
    }

freshSession = Session freshStint

finalizeLap :: Int -> Lap -> Lap
finalizeLap lastTime lap = let
    sectors = lap ^. sectorTimes ++ [0]
    sectorDiffs = reverse $ zipWith (-) (lastTime:sectors) sectors
    in
        lap & lapTime .~ lastTime
            & sectorTimes .~ sectorDiffs

isFullLap :: Lap -> Bool
isFullLap lap = length (lap ^. sectorTimes) == 3

startFreshLap :: StintState -> StintState
startFreshLap = (& currentLap .~ freshLap)
              . (& sector .~ 0)

updateLapState :: Monad m
               => GraphicsPage
               -> StateT StintState m (Maybe LapEvent)
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

                   if isFullLap newLap
                       then return $ Just $ FinishedLap newLap
                       else return Nothing
                else return Nothing

swapState :: MonadState s m => s -> m s
swapState s = get >>= (put s >>) . return

updateStintState :: Monad m
                 => PhysicsPage
                 -> GraphicsPage
                 -> StateT Session m (Maybe SessionEvent)
updateStintState pp gp = do
    let distanceTraveled = gp ^. graphicsPageDistanceTraveled
    lastDistanceTraveled <- zoom (currentStint . stintDistance)
                          $ swapState distanceTraveled

    if distanceTraveled < lastDistanceTraveled
       -- new stint
       then modify (& currentStint .~ freshStint)
         >> return (Just NewStintEvent)
       else zoom currentStint $ updateLapState gp
        >>= return . fmap LapEv


data LapEvent = FinishedSector Int
              | FinishedLap Lap
              deriving Show

data SessionEvent = NewStintEvent
                  | LapEv LapEvent
                  deriving Show
