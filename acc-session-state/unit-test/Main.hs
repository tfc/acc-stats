{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE TypeFamilies      #-}

import           Acc.Session.State
import           Acc.StatsPage
import           Control.Lens.Operators
import           Control.Monad              (forM)
import           Control.Monad.State.Strict
import           Data.Aeson
import           Data.List                  (sort)
import           Data.Maybe                 (fromJust)
import           GHC.Generics
import           System.Directory           (listDirectory)
import           Test.Hspec

data DataPoint = DataPoint
    { _dataGraphics :: GraphicsPage
    , _dataStat     :: StatPage
    , _dataPhysics  :: PhysicsPage
    } deriving (Generic, Show)

dataPointJsonOptions :: Options
dataPointJsonOptions = let l = length ("_data" :: String)
  in defaultOptions { fieldLabelModifier = camelTo2 '_' . drop l }

instance FromJSON DataPoint where
  parseJSON = genericParseJSON dataPointJsonOptions

readBrandsHatch3Data :: IO [DataPoint]
readBrandsHatch3Data = (fromJust <$>)
                     . decodeFileStrict'
                     $ "test-data/brands-hatch-3-laps.json"

-- last lap is displayed first
brandsHatch3Laptimes :: [Lap]
brandsHatch3Laptimes =
    [ Lap { _sectorTimes = [28245,22567,39230]
          , _lapTime = 90042
          , _lapValid = False
          , _inLap = False
          , _outLap = False
          }
    , Lap { _sectorTimes = [28235,22785,37180]
          , _lapTime = 88200
          , _lapValid = True
          , _inLap = False
          , _outLap = False
          }
    , Lap { _sectorTimes = [73391,23017,37400]
          , _lapTime = 133808
          , _lapValid = True
          , _inLap = False
          , _outLap = True
          }
    ]

sessionStateSpec :: Spec
sessionStateSpec = do
    describe "Test Run Data" $ do
      it "from Brands Hatch 3 Laps is correct" $ do
          inputs <- readBrandsHatch3Data
          s <- flip execStateT freshStint $
              forM inputs $ updateLapState . _dataGraphics
          zipWithM_ shouldBe (s ^. finishedLaps) brandsHatch3Laptimes

main :: IO ()
main = hspec sessionStateSpec
