{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

import           Acc.StatsPage
import qualified Acc.StatsPage.Examples as Ex
import           Control.Lens           ((^.))
import           Test.Hspec

main :: IO ()
main = hspec $ do
    describe "physicspage example" $ let p = (Ex.graphicsPage ^.) in do
        it "should contain correct values" $ do
            p graphicsPageStrategyTyreSet `shouldBe` 2
            p graphicsPageDeltaLapTime `shouldBe` "-:--:---"
            p graphicsPageMfdTyrePressureLR `shouldBe` 25.6
            p graphicsPageTrackStatus `shouldBe` "OPTIMUM"
            p graphicsPageTyreCompound `shouldBe` "dry_compound"

    describe "StatPage example" $ let p = (Ex.statPage ^.) in do
        it "should contain correct values" $ do
            p statPageAidAllowTyreBlankets `shouldBe` 0
            p statPageTrack `shouldBe` "Hungaroring"
            p statPageDryTyresName `shouldBe` "DHD2"
            p statPageWetTyresName `shouldBe` "WH"
            p statPageNick `shouldBe` "Jonge"
    describe "PhysicsPage example" $ let p = (Ex.physicsPage ^.) in do
        it "should contain correct values" $ do
            p physicsPagePitLimiterOn `shouldBe` 1
            p physicsPageIsAIControlled `shouldBe` 0
            p physicsPageAbsVibrations `shouldBe` 0
            p physicsPageRearBrakeCompound `shouldBe` 1
            p physicsPageGear `shouldBe` 1
