{-# LANGUAGE FlexibleContexts #-}
module Acc.StatsPage.Binary where

import           Acc.StatsPage
import           Data.Binary.Get
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Vector.Binary   (genericGetVectorWith)
import           Data.Vector.Storable (Vector)

getGraphicsPage :: Get GraphicsPage
getGraphicsPage = let
        utfString n = T.takeWhile (/= '\0') . TE.decodeUtf16LE
                  <$> getByteString (2 * n)
        cInt = fromIntegral <$> getWord32le
        floatVector :: Int -> Get (Vector Float)
        floatVector n = genericGetVectorWith (pure n) getFloatle
        intVector n = genericGetVectorWith (pure n) cInt
    in GraphicsPage
    <$> cInt -- int packetId
    <*> cInt -- ACC_STATUS int enum status
    <*> cInt -- ACC_SESSION_TYPE int enum session
    <*> utfString 15 -- wchar_t currentTime[15]
    <*> utfString 15 -- wchar_t lastTime[15]
    <*> utfString 15 -- wchar_t bestTime[15]
    <*> utfString 15 -- wchar_t split[15]
    <*> cInt -- int completedLaps
    <*> cInt -- int position
    <*> cInt -- int iCurrentTime
    <*> cInt -- int iLastTime
    <*> cInt -- int iBestTime
    <*> getFloatle -- float sessionTimeLeft
    <*> getFloatle -- float distanceTraveled
    <*> cInt -- int isInPit
    <*> cInt --int currentSectorIndex
    <*> cInt -- int lastSectorTime
    <*> cInt -- int numberOfLaps
    <*> utfString 33 -- wchar_t tyreCompound[33]
    <*> ((getWord16le *> getFloatle) -- replayTimeMultiplier unused
     >> getFloatle) -- float normalisedCarPosition
    <*> cInt -- int activeCars
    <*> floatVector (60 * 3) -- float carCoordinates[60][3]
    <*> intVector 60 -- int carId[60]
    <*> cInt -- int activeCars
    <*> getFloatle -- float penaltyTime
    <*> cInt -- ACC_FLAG_TYPE flag
    <*> cInt -- ACC_PENALTY_TYPE penalty
    <*> cInt -- int idealLineOn
    <*> cInt -- int isInPitLane
    <*> (getFloatle -- float surfaceGrip -- always zero
     >> cInt) -- int mandatoryPitDone
    <*> getFloatle -- float windSpeed
    <*> getFloatle -- float windDirection
    <*> cInt -- int isSetupMenuVisible
    <*> cInt -- int mainDisplayIndex
    <*> cInt -- int secondaryDisplayIndex
    <*> cInt -- int tc
    <*> cInt -- int tccut
    <*> cInt -- int engineMap
    <*> cInt -- int abs
    <*> getFloatle -- float fuelXLap
    <*> cInt -- int rainLights
    <*> cInt -- int flashingLights
    <*> cInt -- int lightsStage
    <*> getFloatle -- float exhaustTemperature
    <*> cInt -- int wiperLv
    <*> cInt -- int driverStintTotalTimeLeft
    <*> cInt -- int driverStintTimeLeft
    <*> cInt -- int rainTyres
    <*> cInt -- int sessionIndex
    <*> getFloatle -- float usedFuel
    <*> utfString 15 -- wchar_t deltaLapTime[15]
    <*> (getWord16le >> cInt) -- int iDeltaLapTime
    <*> utfString 15 -- wchar_t estimatedLapTime[15]
    <*> (getWord16le >> cInt) -- int iEstimatedLapTime
    <*> cInt -- int isDeltaPositive
    <*> cInt -- int iSplit
    <*> cInt -- int isValidLap
    <*> getFloatle -- float fuelEstimatedLaps
    <*> utfString 33 -- wchar_t trackStatus[33]
    <*> (getWord16le >> cInt) -- int missingMandatoryPits
    <*> getFloatle -- float clock
    <*> cInt -- int directionLightsLeft
    <*> cInt -- int directionLightsRight
    <*> cInt -- int globalYellow
    <*> cInt -- int globalYellow1
    <*> cInt -- int globalYellow2
    <*> cInt -- int globalYellow3
    <*> cInt -- int globalWhite
    <*> cInt -- int globalGreen
    <*> cInt -- int globalChequered
    <*> cInt -- int globalRed
    <*> cInt -- int mfdTyreSet
    <*> getFloatle -- float mfdFuelToAdd
    <*> getFloatle -- float mfdTyrePressureLF
    <*> getFloatle -- float mfdTyrePressureRF
    <*> getFloatle -- float mfdTyrePressureLR
    <*> getFloatle -- float mfdTyrePRessureRR
    <*> cInt -- ACC_TRACK_GRIP_STATUS trackGripStatus
    <*> cInt -- ACC_RAIN_INTENSITY rainIntensity
    <*> cInt -- ACC_RAIN_INTENSITY rainIntensityIn10min
    <*> cInt -- ACC_RAIN_INTENSITY rainIntensityIn30min
    <*> cInt -- int currentTyreSet
    <*> cInt -- int strategyTyreSet

getPhysicsPage :: Get PhysicsPage
getPhysicsPage = let
        cInt = fromIntegral <$> getWord32le
        floatVector :: Int -> Get (Vector Float)
        floatVector n = genericGetVectorWith (pure n) getFloatle
    in PhysicsPage
    <$> cInt -- int packetId
    <*> getFloatle -- float gas
    <*> getFloatle -- float brake
    <*> getFloatle -- float fuel
    <*> cInt -- int gear
    <*> cInt -- int rpms
    <*> getFloatle -- float steerAngle
    <*> getFloatle -- float speedKmh
    <*> floatVector 3 -- float velocity[3]
    <*> floatVector 3 -- float accG[3]
    <*> floatVector 4 -- float wheelSlip[4]
    <*> (floatVector 4 -- float wheelLoad[4] unused
     >> floatVector 4) -- float wheelPressure[4]
    <*> floatVector 4 -- float wheelAngularSpeed[4]
    <*> (floatVector 4 -- float tyreWear[4] unused
     >> floatVector 4 -- float tyreDirtyLevel[4] unused
     >> floatVector 4) -- float tyreCoreTemp[4]
    <*> (floatVector 4 -- float camberRad[4] unused
     >> floatVector 4) -- float suspensionTravel[4]
    <*> (getFloatle -- float drs unused
     >> getFloatle) -- float tc
    <*> getFloatle -- float heading
    <*> getFloatle -- float pitch
    <*> getFloatle -- float roll
    <*> (getFloatle -- float cgHeight unused
     >> floatVector 5) -- float carDamage[5]
    <*> (cInt -- int numberOfTyresOut unused
     >> cInt) -- int pitLimiterOn
    <*> getFloatle -- float abs
    <*> (getFloatle -- float kersCharge unused
     >> getFloatle --float kersInput unused
     >> cInt) -- int autoShifterOn
    <*> (floatVector 2 -- float rideHeight[2] unused
     >> getFloatle) -- float turboBoost
    <*> (getFloatle -- float ballast unused
     >> getFloatle -- float airDensity unused
     >> getFloatle) -- float airTemp
    <*> getFloatle -- float roadTemp
    <*> floatVector 3 -- float localAngularVel[3]
    <*> getFloatle -- float finalFF
    <*> (getFloatle -- float performanceMeter unused
     >> cInt -- int engineBrake unused
     >> cInt -- int ersRecoveryLevel unused
     >> cInt -- int ersPowerLevel unused
     >> cInt -- int ersHeatCharging unused
     >> cInt -- int ersIsCharging unused
     >> getFloatle -- float kersCurrentKJ unused
     >> cInt -- int drsAvailable unused
     >> cInt -- int drsEnabled unused
     >> floatVector 4) -- float brakeTemp[4]
    <*> getFloatle -- float clutch
    <*> (floatVector 4 -- float tyreTempI[4] unused
     >> floatVector 4 -- float tyreTempM[4] unused
     >> floatVector 4 -- float tyreTempO[4] unused
     >> cInt) -- float isAIControlled
    <*> floatVector 12 -- float tyreContactPoint[4][3]
    <*> floatVector 12 -- float tyreContactNormal[4][3]
    <*> floatVector 12 -- float tyreContactHeading[4][3]
    <*> getFloatle -- float brakeBias
    <*> floatVector 3 -- float localVelocity[3]
    <*> (cInt -- int P2PActivations unused
     >> cInt -- int P2PStatus unused
     >> cInt -- float currentMaxRpm unused
     >> floatVector 4 -- float mz[4] unused
     >> floatVector 4 -- float fx[4] unused
     >> floatVector 4 -- float fy[4] unused
     >> floatVector 4) -- float slipRatio[4]
    <*> floatVector 4 -- float slipAngle[4]
    <*> (cInt -- int tcinAction unused
     >> cInt -- int absInAction unused
     >> floatVector 4 -- float suspensionDamage[4] unused
     >> floatVector 4 -- float tyreTemp[4] unused
     >> getFloatle) -- float waterTemp
    <*> floatVector 4 -- float brakePressure[4]
    <*> cInt -- int frontBrakeCompound
    <*> cInt -- int rearBrakeCompound
    <*> floatVector 4 -- float padLife[4]
    <*> floatVector 4 -- float discLife[4]
    <*> cInt -- int ignitionOn
    <*> cInt -- int starterEngineOn
    <*> cInt -- int isEngineRunning
    <*> getFloatle -- float kerbVibration
    <*> getFloatle -- float slipVibrations
    <*> getFloatle -- float gVibrations
    <*> getFloatle -- float absVibrations

getStatPage :: Get StatPage
getStatPage = let
        utfString n = T.takeWhile (/= '\0') . TE.decodeUtf16LE
                  <$> getByteString (2 * n)
        cInt = fromIntegral <$> getWord32le
        floatVector :: Int -> Get (Vector Float)
        floatVector n = genericGetVectorWith (pure n) getFloatle
    in StatPage
    <$> utfString 15 -- wchar_t smVersion[15]
    <*> utfString 15 -- wchar_t acVersion[15]
    <*> cInt -- int numberOfSessions
    <*> cInt -- int numCars
    <*> utfString 33 -- wchar_t carModel[33]
    <*> utfString 33 -- wchar_t track[33]
    <*> utfString 33 -- wchar_t playerName[33]
    <*> utfString 33 -- wchar_t playerSurname[33]
    <*> utfString 33 -- wchar_t playerNick[33]
    <*> (getWord16le *> cInt) -- int sectorCount
    <*> (getFloatle -- float maxTorque unused
     >> getFloatle -- float maxPower unused
     >> cInt) -- int maxRpm
    <*> getFloatle -- float maxFuel
    <*> (floatVector 4 -- float suspensionMaxTravel[4] unused
     >> floatVector 4 -- float tyreRadius[4] unused
     >> getFloatle -- float maxTurboBoost unused
     >> getFloatle -- float deprecated1 unused
     >> getFloatle -- float deprecated2 unused
     >> cInt) -- int penaltiesEnabled
    <*> getFloatle -- float aidFuelRate
    <*> getFloatle -- float aidTireRate
    <*> getFloatle -- float aidMechanicalDamage
    <*> getFloatle -- float allowTyreBlankets
    <*> getFloatle -- float aidStability
    <*> cInt -- int aidAutoclutch
    <*> cInt -- int aidAutoBlip
    <*> (cInt -- int hasDRS unused
     >> cInt -- int hasERS unused
     >> cInt -- int hasKERS unused
     >> getFloatle -- float kersMaxJ unused
     >> cInt -- int engineBrakeSettingsCount unused
     >> cInt -- int ersPowerControllerCount unused
     >> getFloatle -- float trackSplineLength unused
     >> utfString 33 -- wchar_t trackConfiguration[33] unused
     >> (getWord16le *> getFloatle) -- float ersMaxJ unused
     >> cInt -- int isTimedRace unused
     >> cInt -- int hasExtraLap unused
     >> utfString 33 -- wchar_t carSkin[33] unused
     >> (getWord16le *> cInt) -- int reversedGridPositions unused
     >> cInt) -- int pitWindowStart
    <*> cInt -- int pitWindowEnd
    <*> cInt -- int isOnline
    <*> utfString 33 -- wchar_t dryTyresName[33]
    <*> utfString 33 -- wchar_t wetTyresName[33]

-- this obviously only works on Get functions that don't check anything
structureSize :: Get a -> Int
structureSize x = fromIntegral $ runGet (x >> bytesRead) (BL.repeat 0)

readStructure :: Get a -> BS.ByteString -> a
readStructure getter = runGet getter . BL.fromStrict
