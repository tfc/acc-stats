{-# LANGUAGE CPP #-}

module Acc.Stats.Client where

import           Acc.Stats.API
import           Data.Proxy          (Proxy (..))
import           Network.HTTP.Client hiding (Proxy)
import           Servant.API
import           Servant.Client

getFunctionPostDataPoint :: IO (DataPoint -> IO (Either ClientError ()))
getFunctionPostDataPoint = do
    let (_ :<|> postDataPointRoute :<|> _) = client (Proxy :: Proxy AccStatsApi)
    m <- newManager defaultManagerSettings
    let clientEnv = mkClientEnv m (BaseUrl Http "localhost" 8000 "")
        postDataPoint dp = runClientM (postDataPointRoute dp) clientEnv
    return postDataPoint
