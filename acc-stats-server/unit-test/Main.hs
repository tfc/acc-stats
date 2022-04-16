{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Acc.Stats.API
import           Acc.Stats.Server
import qualified Acc.StatsPage.Examples as Ex
import qualified Data.Map.Strict          as HM
import           Network.HTTP.Client      hiding (Proxy, port)
import qualified Network.Wai.Handler.Warp as Warp
import           Servant hiding (Get)
import           Servant.Client hiding (baseUrl, manager)
import           Test.Hspec

app :: TC -> Application
app = serve accStatsApiProxy . server

withUserApp :: (Warp.Port -> IO ()) -> IO ()
withUserApp action = do
    s <- emptyServerState
    Warp.testWithApplication (pure (app s)) action

exDP :: DataPoint
exDP = DataPoint Ex.graphicsPage Ex.statPage Ex.physicsPage

businessLogicSpec :: Spec
businessLogicSpec = around withUserApp $ do
    let (listTC :<|> postDataPoint :<|> _) = client (Proxy :: Proxy AccStatsApi)
    baseUrl <- runIO $ parseBaseUrl "http://localhost"
    manager <- runIO $ newManager defaultManagerSettings
    let clientEnv port = mkClientEnv manager (baseUrl { baseUrlPort = port })

    let
        returns :: forall a. (Show a, Eq a)
                => (ClientM a, Warp.Port)
                -> Either ClientError a
                -> IO ()
        returns (endpointCall, port) result =
            runClientM endpointCall (clientEnv port) >>= (`shouldBe` result)

    describe "GET /" $ do
      it "should present an empty TC at the beginning" $ \port -> do
        result <- runClientM listTC (clientEnv port)
        result `shouldBe` (Right $ HM.empty)
      it "posting of a datapoint should work" $ \port -> do
        (postDataPoint exDP, port) `returns` Right ()
        (listTC, port) `returns` Right (HM.fromList [("Jacek Jonge Jonge", exDP)])

main :: IO ()
main = hspec businessLogicSpec
