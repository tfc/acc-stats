{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Acc.StatsPage where

import           Control.Lens.TH
import           Data.Aeson
import           Data.Text       (Text)
import           GHC.Generics    (Generic)
import qualified Data.Vector.Storable as V

data GraphicsPage = GraphicsPage
    { _graphicsPagePacketId                 :: !Int
    , _graphicsPageStatus                   :: !Int -- should be AccStatus enum type
    , _graphicsPageSession                  :: !Int -- should be AccSessionType enum type
    , _graphicsPageCurrentTime              :: !Text
    , _graphicsPageLastTime                 :: !Text
    , _graphicsPageBestTime                 :: !Text
    , _graphicsPageSplit                    :: !Text
    , _graphicsPageCompletedLaps            :: !Int
    , _graphicsPagePosition                 :: !Int
    , _graphicsPageICurrentTimeMs           :: !Int
    , _graphicsPageILastTimeMs              :: !Int
    , _graphicsPageIBestTimeMs              :: !Int
    , _graphicsPageSessionTimeLeft          :: !Float
    , _graphicsPageDistanceTraveled         :: !Float
    , _graphicsPageIsInPit                  :: !Int
    , _graphicsPageCurrentSectorIndex       :: !Int
    , _graphicsPageLastSectorTime           :: !Int
    , _graphicsPageNumberOfLaps             :: !Int
    , _graphicsPageTyreCompound             :: !Text
    , _graphicsPageNormalizedCarPosition    :: !Float
    , _graphicsPageActiveCars               :: !Int
    , _graphicsPageCarCoordinates           :: !(V.Vector Float) -- items: 60 * 3
    , _graphicsPageCarID                    :: !(V.Vector Int) -- items: 60
    , _graphicsPagePlayerCarID              :: !Int
    , _graphicsPagePenaltyTime              :: !Float
    , _graphicsPageFlag                     :: !Int -- should be AccFlagType enum type
    , _graphicsPagePenalty                  :: !Int -- should be AccPenaltyType enum type
    , _graphicsPageIdealLineOn              :: !Int
    , _graphicsPageIsInPitLane              :: !Int
    , _graphicsPageMandatoryPitDone         :: !Int
    , _graphicsPageWindSpeedMetersPerSecond :: !Float
    , _graphicsPageWindDirection            :: !Float
    , _graphicsPageIsSetupMenuVisible       :: !Int
    , _graphicsPageMainDisplayIndex         :: !Int
    , _graphicsPageSecondaryDisplayIndex    :: !Int
    , _graphicsPageTC                       :: !Int
    , _graphicsPageTCCut                    :: !Int
    , _graphicsPageEngineMap                :: !Int
    , _graphicsPageABS                      :: !Int
    , _graphicsPageFuelXLap                 :: !Float
    , _graphicsPageRainLights               :: !Int
    , _graphicsPageFlashingLights           :: !Int
    , _graphicsPageLightsStage              :: !Int
    , _graphicsPageExhaustTemperature       :: !Float
    , _graphicsPageWiperLV                  :: !Int
    , _graphicsPageDriverStintTotalTimeLeft :: !Int
    , _graphicsPageDriverStintTimeLeft      :: !Int
    , _graphicsPageRainTyres                :: !Int
    , _graphicsPageSessionIndex             :: !Int
    , _graphicsPageUsedFuel                 :: !Float
    , _graphicsPageDeltaLapTime             :: !Text
    , _graphicsPageIDeltaLapTimeMs          :: !Int
    , _graphicsPageEstimatedLapTime         :: !Text
    , _graphicsPageIEstimatedLapTime        :: !Int
    , _graphicsPageIsDeltaPositive          :: !Int
    , _graphicsPageISplit                   :: !Int
    , _graphicsPageIsValidLap               :: !Int
    , _graphicsPageFuelEstimatedLaps        :: !Float
    , _graphicsPageTrackStatus              :: !Text
    , _graphicsPageMissingMandatoryPits     :: !Int
    , _graphicsPageClock                    :: !Float -- time of day in seconds
    , _graphicsPageDirectionLightsLeft      :: !Int
    , _graphicsPageDirectionLightsRight     :: !Int
    , _graphicsPageGlobalYellow             :: !Int
    , _graphicsPageGlobalYellow1            :: !Int
    , _graphicsPageGlobalYellow2            :: !Int
    , _graphicsPageGlobalYellow3            :: !Int
    , _graphicsPageGlobalWhite              :: !Int
    , _graphicsPageGlobalGreen              :: !Int
    , _graphicsPageGlobalChequered          :: !Int
    , _graphicsPageGlobalRed                :: !Int
    , _graphicsPageMfdTyreSet               :: !Int
    , _graphicsPageMfdFuelToAdd             :: !Float
    , _graphicsPageMfdTyrePressureLF        :: !Float
    , _graphicsPageMfdTyrePressureRF        :: !Float
    , _graphicsPageMfdTyrePressureLR        :: !Float
    , _graphicsPageMfdTyrePressureRR        :: !Float
    , _graphicsPageTrackGripStatus          :: !Int -- should be AccTrackGripStatus enum type
    , _graphicsPageRainIntensity            :: !Int -- should be AccRainIntensity enum type
    , _graphicsPageRainIntensityIn10min     :: !Int -- should be AccRainIntensity enum type
    , _graphicsPageRainIntensityIn30min     :: !Int -- should be AccRainIntensity enum type
    , _graphicsPageCurrentTyreSet           :: !Int
    , _graphicsPageStrategyTyreSet          :: !Int
    } deriving (Eq, Generic, Show)


graphicsJsonOptions :: Options
graphicsJsonOptions = let l = length ("_graphicsPage" :: String)
  in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance FromJSON GraphicsPage where
  parseJSON = genericParseJSON graphicsJsonOptions
instance ToJSON GraphicsPage where
  toJSON = genericToJSON graphicsJsonOptions

data PhysicsPage = PhysicsPage
    { _physicsPagePacketId            :: !Int
    , _physicsPageGas                 :: !Float
    , _physicsPageBrake               :: !Float
    , _physicsPageFuel                :: !Float
    , _physicsPageGear                :: !Int
    , _physicsPageRpms                :: !Int
    , _physicsPageSteerAngle          :: !Float
    , _physicsPageSpeedKmh            :: !Float
    , _physicsPageVelocity            :: !(V.Vector Float) -- items: 3
    , _physicsPageAccG                :: !(V.Vector Float) -- items: 3
    , _physicsPageWheelSlip           :: !(V.Vector Float) -- items: 4
    , _physicsPageWheelsPressure      :: !(V.Vector Float) -- items: 4
    , _physicsPageWheelAngularSpeed   :: !(V.Vector Float) -- items: 4
    , _physicsPageTyreCoreTemperature :: !(V.Vector Float) -- items: 4
    , _physicsPageSuspensionTravel    :: !(V.Vector Float) -- items: 4
    , _physicsPageTc                  :: !Float
    , _physicsPageHeading             :: !Float
    , _physicsPagePitch               :: !Float
    , _physicsPageRoll                :: !Float
    , _physicsPageCarDamage           :: !(V.Vector Float) -- items: 5 [front, rear, left, right, centre]
    , _physicsPagePitLimiterOn        :: !Int
    , _physicsPageAbs                 :: !Float
    , _physicsPageAutoShifterOn       :: !Int
    , _physicsPageTurboBoost          :: !Float
    , _physicsPageAirTemp             :: !Float
    , _physicsPageRoadTemp            :: !Float
    , _physicsPageLocalAngularVel     :: !(V.Vector Float) -- items: 3
    , _physicsPageFinalFF             :: !Float
    , _physicsPageBrakeTemp           :: !(V.Vector Float) -- items: 4
    , _physicsPageClutch              :: !Float
    , _physicsPageIsAIControlled      :: !Int
    , _physicsPageTyreContactPoint    :: !(V.Vector Float) -- items: 4 * 3
    , _physicsPageTyreContactNormal   :: !(V.Vector Float) -- items: 4 * 3
    , _physicsPageTyreContactHeading  :: !(V.Vector Float) -- items: 4 * 3
    , _physicsPageBrakeBias           :: !Float
    , _physicsPageLocalVelocity       :: !(V.Vector Float) -- items: 3
    , _physicsPageSlipRatio           :: !(V.Vector Float) -- items: 4
    , _physicsPageSlipAngle           :: !(V.Vector Float) -- items: 4
    , _physicsPageWaterTemp           :: !Float
    , _physicsPageBrakePressure       :: !(V.Vector Float) -- items: 4
    , _physicsPageFrontBrakeCompound  :: !Int
    , _physicsPageRearBrakeCompound   :: !Int
    , _physicsPagePadLife             :: !(V.Vector Float) -- items: 4
    , _physicsPageDiscLife            :: !(V.Vector Float) -- items: 4
    , _physicsPageIgnitionOn          :: !Int
    , _physicsPageStarterEngineOn     :: !Int
    , _physicsPageIsEngineRunning     :: !Int
    , _physicsPageKerbVibration       :: !Float
    , _physicsPageSlipVibrations      :: !Float
    , _physicsPageGVibrations         :: !Float
    , _physicsPageAbsVibrations       :: !Float
    } deriving (Eq, Generic, Show)

-- comment on the damage:
-- Forum guy says https://www.assettocorsa.net/forum/index.php?threads/acc-shared-memory-documentation.59965/page-14#post-1190502
-- "I did some proper trial and error stuff and ended up converting the number
--  to seconds and dividing it by 3.54168. It`s fairly accurate to within .001,
--  but only for body damage. As soon as suspension damage occurs, its out
--  unless you turn off repairing it in the mfd."

physicsJsonOptions :: Options
physicsJsonOptions = let l = length ("_physicsPage" :: String)
        in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance FromJSON PhysicsPage where
  parseJSON = genericParseJSON physicsJsonOptions
instance ToJSON PhysicsPage where
  toJSON = genericToJSON physicsJsonOptions

data StatPage = StatPage
    { _statPageSmVersion                :: !Text
    , _statPageAcVersion                :: !Text
    , _statPageSessions                 :: !Int
    , _statPageCars                     :: !Int
    , _statPageCarModel                 :: !Text
    , _statPageTrack                    :: !Text
    , _statPagePlayerName               :: !Text
    , _statPagePlayerSurname            :: !Text
    , _statPageNick                     :: !Text
    , _statPageSectorCount              :: !Int
    , _statPageMaxRpm                   :: !Int
    , _statPageMaxFuel                  :: !Float
    , _statPagePenaltiesEnabled         :: !Int
    , _statPageAidFuelRate              :: !Float
    , _statPageAidTireRate              :: !Float
    , _statPageAidTechnicalDamage       :: !Float
    , _statPageAidAllowTyreBlankets     :: !Float
    , _statPageAidStability             :: !Float
    , _statPageAidAutoClutch            :: !Int
    , _statPageAidAutoBlip              :: !Int
    , _statPagePitWindowStart           :: !Int
    , _statPagePitWindowEnd             :: !Int
    , _statPageIsOnline                 :: !Int
    , _statPageDryTyresName             :: !Text
    , _statPageWetTyresName             :: !Text
    } deriving (Eq, Generic, Show)


statJsonOptions :: Options
statJsonOptions = let l = length ("_statPage" :: String)
  in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance FromJSON StatPage where
  parseJSON = genericParseJSON statJsonOptions
instance ToJSON StatPage where
  toJSON = genericToJSON statJsonOptions

$(makeLenses ''GraphicsPage)
$(makeLenses ''PhysicsPage)
$(makeLenses ''StatPage)

data FullData = FullData GraphicsPage PhysicsPage StatPage
