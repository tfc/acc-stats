{-# LANGUAGE OverloadedStrings #-}
module Acc.Stats.Server where

import           Acc.Session.DataStructures
import           Acc.Stats.API
import           Control.Monad.IO.Class     (liftIO)
import           Servant.Server
import           Servant.Server.Generic

record :: SessionRoutes AsServer
record = SessionRoutes
    { _postNewSession = pure 123
    , _putNewEvent = f
    }

f :: Int -> SessionEvent -> Handler ()
f sessionId event = do
    liftIO $ do
        print sessionId
        print event
    return ()

serverApp :: Application
serverApp = genericServe record
