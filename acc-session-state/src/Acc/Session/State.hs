{-# LANGUAGE BangPatterns    #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}
module Acc.Session.State where

import           Acc.Session.DataStructures
import           Acc.StatsPage
import           Control.Lens.Combinators
import           Control.Lens.Operators
import           Control.Monad.State.Strict
import           Data.Maybe                 (catMaybes)
import           Data.Time                  (getCurrentTime)
import qualified Data.Vector.Unboxed        as V
import           GHC.Generics
import           Timestamp                  (Timestamp, utcTimeTimestamp)

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
    , _currentLap      :: !Lap
    , _lapTelemetry    :: ![LapTelemetry]
    , _currentSector   :: !Int
    , _lastTelemDistance :: !Float
    } deriving (Generic, Show)

makeLenses ''StintState

freshStintState :: GraphicsPage -> GraphicsPage -> StintState
freshStintState oldGp newGp = StintState
    { _lastGraphics = oldGp
    , _currentGraphics = newGp
    , _currentLap = freshLap
    , _lapTelemetry = []
    , _currentSector = -1
    , _lastTelemDistance = 0.0
    }

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
eventsFromData last next = mconcat $ map (\f -> f last next)
        [ pitLaneEntry
        , pitLaneExit
        , sectorInvalidated
        , nextLapOrSector
        , nextStint
        ]
    where
      isInPitlane = (/= 0) . (^. graphicsPageIsInPitLane)
      isValid = (/= 0) . (^. graphicsPageIsValidLap)

      pitLaneEntry a b = [PitlaneEntry | not (isInPitlane a) && isInPitlane b]
      pitLaneExit a b = [PitlaneExit | isInPitlane a && not (isInPitlane b)]
      -- currentSectorIndex is not up2date if driver teleported into pit and then continues driving
      -- so let's check the sector time change first and then see if we're entering a new lap or not
      nextLapOrSector a b = if  a ^. graphicsPageLastSectorTime /= b ^. graphicsPageLastSectorTime
                                then if b ^. graphicsPageCurrentSectorIndex == 0 then [NextLap] else [NextSector]
                                else []
      nextStint a b = [NextStint | _graphicsPageDistanceTraveled a > _graphicsPageDistanceTraveled b]
      sectorInvalidated a b = [SectorInvalidated | isValid a && not (isValid b)]

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

shiftGraphicsInput :: Monad m => GraphicsPage -> StateT StintState m ()
shiftGraphicsInput nextGp = do
    lastGp <- gets (^. currentGraphics)
    modify $ (& lastGraphics .~ lastGp)
           . (& currentGraphics .~ nextGp)

currentLapTelemetry :: Timestamp -> Float -> PhysicsPage -> LapTelemetry
currentLapTelemetry ts pos p = LapTelemetry
    { _telTimestamp = ts
    , _telNormPosition = pos
    , _telGas = _physicsPageGas p
    , _telBrake = _physicsPageBrake p
    , _telGear = _physicsPageGear p
    , _telRpms = _physicsPageRpms p
    , _telSpeed = _physicsPageSpeedKmh p
    , _telSteerAngle = _physicsPageSteerAngle p
    , _telWheelPressures = V.toList $ _physicsPageWheelsPressure p
    , _telWheelTemps = V.toList $ _physicsPageTyreCoreTemperature p
    }

updateTelemetry :: MonadIO m => PhysicsPage -> StateT StintState m ()
updateTelemetry pp = do
    let speed = pp ^. physicsPageSpeedKmh
        gas = pp ^. physicsPageGas
        brake = pp ^. physicsPageBrake
    pos <- gets (^. currentGraphics . graphicsPageNormalizedCarPosition)
    now <- liftIO getCurrentTime
    let !telemetryPoint = currentLapTelemetry (utcTimeTimestamp now) pos pp
    modify (& lapTelemetry %~ (telemetryPoint:))

updateStint :: MonadIO m
          => GraphicsPage
          -> PhysicsPage
          -> StateT StintState m [SessionEvent]
updateStint nextGp pp = do
    lastGp <- gets (^. currentGraphics)
    let events = eventsFromData lastGp nextGp
    shiftGraphicsInput nextGp
    rets <- catMaybes <$> mapM updateState events

    -- only update telemetry after traveling at least 5m
    let currentDist = nextGp ^. graphicsPageDistanceTraveled
    lastDist <- gets (^. lastTelemDistance)
    when (currentDist - lastDist > 5.0) $ do
        updateTelemetry pp
        modify (& lastTelemDistance .~ currentDist)

    return rets

updateState :: Monad m
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

    lapTelemetry <- reverse <$> zoom lapTelemetry (swapState [])
    if isFullLap finishedLap
        then return $ Just $ FinishedLap finishedLap lapTelemetry
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
