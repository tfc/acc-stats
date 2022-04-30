{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.State where

import           Acc.StatsPage
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Lens.TH
import           Control.Monad.State.Strict
import           Data.Maybe                 (catMaybes)
import           GHC.Generics

data Lap = Lap
    { _sectorTimes :: [Int]
    , _lapTime     :: Int
    , _lapValid    :: Bool
    , _inLap       :: Bool
    , _outLap      :: Bool
    } deriving (Eq, Generic, Show)

makeLenses ''Lap

freshLap = Lap
    { _sectorTimes = []
    , _lapTime = 0
    , _lapValid = True
    , _inLap = False
    , _outLap = False
    }

data StintState = StintState
    { _lastGraphics    :: GraphicsPage
    , _currentGraphics :: GraphicsPage
    , _currentLap      :: Lap
    , _currentSector   :: Int
    } deriving (Generic, Show)

makeLenses ''StintState

freshStintState :: GraphicsPage -> GraphicsPage -> StintState
freshStintState oldGp newGp = StintState oldGp newGp freshLap 0

data Event = NextStint
           | NextLap
           | NextSector
           | SectorInvalidated
           | PitlaneEntry
           | PitlaneExit
    deriving (Eq, Show)

-- Theoretically all those events are discrete enough to not happen at the
-- same time, but: If the sampling rate is low enough it looks like they
-- happen at the same time. In what order to handle them is then a matter of
-- definitions.
eventsFromData :: GraphicsPage -> GraphicsPage -> [Event]
eventsFromData last next = mconcat $ map f eventFunctions
    where
      f (condition, result) = [result | condition next last]
      isInPitlane = (/= 0) . (^. graphicsPageIsInPitLane)
      isValid = (/= 0) . (^. graphicsPageIsValidLap)
      eventFunctions =
        [ ((. (not . isInPitlane)) . (&&) . isInPitlane
          , PitlaneEntry)
        , ((. isInPitlane) . (&&) . (not . isInPitlane)
          , PitlaneExit)
        , ((. (^. graphicsPageCurrentSectorIndex) ) .
            (<) . (^. graphicsPageCurrentSectorIndex)
          , NextLap)
        , ((. (^. graphicsPageCurrentSectorIndex) ) .
            (>) . (^. graphicsPageCurrentSectorIndex)
          , NextSector)
        , ((. (^. graphicsPageDistanceTraveled) ) .
            (<) . (^. graphicsPageDistanceTraveled)
          , NextStint)
        , ((. isValid) . (&&) . (not . isValid)
          , SectorInvalidated)
        ]

finalizeLap :: Int -> Lap -> Lap
finalizeLap lastTime lap = let
    sectors = lap ^. sectorTimes ++ [0]
    sectorDiffs = reverse $ zipWith (-) (lastTime:sectors) sectors
    in
        lap & lapTime .~ lastTime
            & sectorTimes .~ sectorDiffs

isFullLap :: Lap -> Bool
isFullLap lap = length (lap ^. sectorTimes) == 3

swapState :: MonadState s m => s -> m s
swapState s = get >>= (put s >>) . return

data SessionEvent = FinishedSector Int
                  | FinishedLap Lap
                  | FinishedStint Float
                  deriving Show

updateStint :: MonadIO m
          => GraphicsPage
          -> StateT StintState m [SessionEvent]
updateStint nextGp = do
    -- TODO SWAP
    lastGp <- gets (^. currentGraphics)
    modify $ (& currentGraphics .~ nextGp)
           . (& lastGraphics .~ lastGp)
    let events = eventsFromData lastGp nextGp
    catMaybes <$> mapM updateState events

updateState :: MonadIO m
            => Event
            -> StateT StintState m (Maybe SessionEvent)

updateState NextStint = do
    -- drop everything but the graphics page
    oldState <- get
    put $ freshStintState (oldState ^. lastGraphics) (oldState ^. currentGraphics)

    -- some laps are invalid from the beginning
    validLap <- (/= 0) <$> gets (^. currentGraphics . graphicsPageIsValidLap)
    modify (& currentLap . lapValid .~ validLap)

    Just . FinishedStint <$> gets (^. lastGraphics . graphicsPageDistanceTraveled)

updateState NextLap = do
    currentLapState <- gets _currentLap
    lastTime <- gets (^. currentGraphics . graphicsPageILastTimeMs)
    let finishedLap = finalizeLap lastTime currentLapState
    modify $ (& currentLap .~ freshLap)
           . (& currentSector .~ 0)
    if isFullLap finishedLap
        then return $ Just $ FinishedLap finishedLap
        else return Nothing

updateState NextSector = do
    lastSector <- gets (^. currentGraphics . graphicsPageCurrentSectorIndex)
    sectorTime <- gets (^. currentGraphics . graphicsPageLastSectorTime)
    modify (& currentLap . sectorTimes %~ (sectorTime:))
    return $ Just $ FinishedSector lastSector

updateState SectorInvalidated = modify (& currentLap . lapValid .~ False)
    >> return Nothing

updateState PitlaneEntry = modify (& currentLap . inLap .~ True)
    >> return Nothing

updateState PitlaneExit = modify (& currentLap . outLap .~ True)
    >> return Nothing
