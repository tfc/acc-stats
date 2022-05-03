{-# LANGUAGE CPP #-}

module Acc.Stats.Client where

import           Acc.Stats.API
import           Control.Exception      (throwIO)
import           Network.HTTP.Client    (Manager)
import           Servant.Client
import           Servant.Client.Generic

accSessionRoutes :: Manager -> BaseUrl -> SessionRoutes (AsClientT IO)
accSessionRoutes mgr burl = genericClientHoist
    (\x -> runClientM x (mkClientEnv mgr burl) >>= either throwIO return)
