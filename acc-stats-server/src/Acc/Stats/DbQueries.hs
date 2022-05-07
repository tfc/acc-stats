{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Acc.Stats.DbQueries where

import           Acc.Session.DataStructures
import           Data.Int                   (Int32)
import           Data.Time                  (UTCTime)
import qualified Hasql.Session              as Session
import           Hasql.Statement            (Statement (..))
import qualified Hasql.TH                   as TH

insertStintStatement :: Statement () Int32
insertStintStatement = [TH.singletonStatement|
    INSERT INTO stints (started_on)
    VALUES (now())
    RETURNING stint_id :: int4
    |]

insertStint :: Session.Session Int32
insertStint = Session.statement () insertStintStatement

insertLapStatement :: Statement (Int32, UTCTime, Bool, Bool, Bool, Int32, Int32, Int32) ()
insertLapStatement = [TH.resultlessStatement|
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
    |]

insertLap :: Int32 -> UTCTime -> Lap -> Session.Session ()
insertLap sid time l = let
    [s1, s2, s3] = fromIntegral <$> _sectorTimes l
    in
    Session.statement (sid, time, _lapValid l, _inLap l, _outLap l, s1, s2, s3) insertLapStatement

insertTelemetryStatement :: Statement (Int32, UTCTime, Float, Float, Float, Float) ()
insertTelemetryStatement = [TH.resultlessStatement|
    INSERT INTO telemetry ( stint_id
                          , timestamp
                          , normpos
                          , speed
                          , gas
                          , brake
                          )
    VALUES ( $1 :: int4
           , $2 :: timestamptz
           , $3 :: float4
           , $4 :: float4
           , $5 :: float4
           , $6 :: float4
           )
    |]

insertTelemetry :: Int32 -> UTCTime -> LapTelemetry -> Session.Session ()
insertTelemetry sid time t =
    Session.statement (sid, time, _telNormPosition t, _telSpeed t, _telGas t, _telBrake t) insertTelemetryStatement
