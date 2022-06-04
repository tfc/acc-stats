{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Acc.Stats.DbQueries where

import           Acc.Session.DataStructures
import           Data.Int                   (Int32)
import           Data.Time                  (UTCTime)
import qualified Hasql.Session              as Session
import           Hasql.Statement            (Statement (..))
import qualified Hasql.TH                   as TH
import           Timestamp                  (timestampUtcTime)

insertStintStatement :: Statement () Int32
insertStintStatement = [TH.singletonStatement|
    INSERT INTO stints (started_on)
    VALUES (now())
    RETURNING stint_id :: int4
    |]

insertStint :: Session.Session Int32
insertStint = Session.statement () insertStintStatement

insertLapStatement :: Statement (Int32, UTCTime, Bool, Bool, Bool, Int32, Int32, Int32) Int32
insertLapStatement = [TH.singletonStatement|
    INSERT INTO laps ( stint_id
                     , finished_on
                     , valid
                     , inlap
                     , outlap
                     , sector1
                     , sector2
                     , sector3
                     )
    VALUES ( $1 :: int4
           , $2 :: timestamptz
           , $3 :: bool
           , $4 :: bool
           , $5 :: bool
           , $6 :: int4
           , $7 :: int4
           , $8 :: int4
           )
    RETURNING lap_id :: int4
    |]

insertLap :: Int32 -> UTCTime -> Lap -> Session.Session Int32
insertLap sid time l = let
    [s1, s2, s3] = fromIntegral <$> _sectorTimes l
    in
    Session.statement (sid, time, _lapValid l, _inLap l, _outLap l, s1, s2, s3) insertLapStatement

insertTelemetryStatement :: Statement (Int32, UTCTime, Float, Float, Float, Int32, Int32, Float, Float, Float, Float, Float, Float, Float, Float, Float, Float) ()
insertTelemetryStatement = [TH.resultlessStatement|
    INSERT INTO telemetry ( lap_id
                          , timestamp
                          , normpos
                          , gas
                          , brake
                          , gear
                          , rpms
                          , speed
                          , steer_angle
                          , wheel_pressure_fl
                          , wheel_pressure_fr
                          , wheel_pressure_rl
                          , wheel_pressure_rr
                          , wheel_temperature_fl
                          , wheel_temperature_fr
                          , wheel_temperature_rl
                          , wheel_temperature_rr
                          )
    VALUES ( $1 :: int4
           , $2 :: timestamptz

           , $3 :: float4
           , $4 :: float4
           , $5 :: float4
           , $6 :: int4
           , $7 :: int4
           , $8 :: float4
           , $9 :: float4

           , $10 :: float4
           , $11 :: float4
           , $12 :: float4
           , $13 :: float4

           , $14 :: float4
           , $15 :: float4
           , $16 :: float4
           , $17 :: float4
           )
    |]

insertTelemetry :: Int32 -> LapTelemetry -> Session.Session ()
insertTelemetry lapId t =
    let
        [p1, p2, p3, p4] = _telWheelPressures t
        [t1, t2, t3, t4] = _telWheelTemps t
    in
    Session.statement ( lapId
                      , timestampUtcTime $ _telTimestamp t
                      , _telNormPosition t
                      , _telGas t
                      , _telBrake t
                      , fromIntegral $ _telGear t
                      , fromIntegral $ _telRpms t
                      , _telSpeed t
                      , _telSteerAngle t
                      , p1, p2, p3, p4
                      , t1, t2, t3, t4
                      ) insertTelemetryStatement
