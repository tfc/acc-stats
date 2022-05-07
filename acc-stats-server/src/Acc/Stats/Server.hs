{-# LANGUAGE OverloadedStrings #-}
module Acc.Stats.Server where

import           Acc.Session.DataStructures
import           Acc.Stats.API
import           Acc.Stats.DbQueries
import           Control.Exception          (throwIO)
import           Control.Monad.Reader
import           Data.Time.Clock            (getCurrentTime)
import           Hasql.Connection           (Connection)
import           Hasql.Session              (Session, run)
import           Servant.Server
import           Servant.Server.Generic

type AppM = ReaderT AppCtx Handler

data AppCtx = AppCtx
    { _getDbConnection :: Connection
    }

runSqlSession :: Session a -> AppM a
runSqlSession s = do
    connection <- _getDbConnection <$> ask
    liftIO (either throwIO return =<< run s connection)

record :: SessionRoutes (AsServerT AppM)
record = SessionRoutes
    { _postNewSession = postNewSession
    , _putNewEvent = putNewEvent
    }

postNewSession :: AppM Int
postNewSession = fromIntegral <$> runSqlSession insertStint

putNewEvent :: Int -> SessionEvent -> AppM ()
putNewEvent sessionId (FinishedLap lap _) = do
    now <- liftIO $ getCurrentTime
    runSqlSession $ insertLap (fromIntegral sessionId) now lap
putNewEvent _ _ = pure ()

serverApp :: AppCtx -> Application
serverApp appCtx = genericServeT (flip runReaderT appCtx) record
