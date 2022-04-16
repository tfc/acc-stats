module Main where

import           Acc.Stats.API
import           Acc.Stats.Client
import qualified Acc.StatsPage.Examples as Ex
import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever, (>=>))

main :: IO ()
main = do
    pd <- getFunctionPostDataPoint
    forever $ do
        print =<< pd (DataPoint Ex.graphicsPage Ex.statPage Ex.physicsPage)
        threadDelay 3000000
