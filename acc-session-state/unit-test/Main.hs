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

readData :: String -> IO [DataPoint]
readData = (fromJust <$>) . decodeFileStrict'

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


readZolder5Data :: IO [DataPoint]
readZolder5Data = (fromJust <$>)
                . decodeFileStrict'
                $ "test-data/zolder-5-laps-tyre-temps.json"

zolder5Laptimes :: [Lap]
zolder5Laptimes =
    [ Lap {_sectorTimes = [31462,30365,30750]
          , _lapTime = 92577
          , _lapValid = True
          , _inLap = False
          , _outLap = False
          }
    , Lap {_sectorTimes = [32132,31223,30800]
          , _lapTime = 94155
          , _lapValid = True
          , _inLap = False
          , _outLap = False
          }
    , Lap {_sectorTimes = [269840,44862,31568]
          , _lapTime = 346270
          , _lapValid = False
          , _inLap = False
          , _outLap = True
          }
    , Lap {_sectorTimes = [32337,32123,30460]
          , _lapTime = 94920
          , _lapValid = True
          , _inLap = False
          , _outLap = False
          }
    , Lap {_sectorTimes = [135763,31543,31627]
          , _lapTime = 198933
          , _lapValid = True
          , _inLap = False
          , _outLap = True
          }
    ]

sessionStateSpec :: Spec
sessionStateSpec = do
    describe "Real-Life Lap Data" $
        let
            lapTimeCheck filename correctLaps =
              it ("lap times from " <> filename <> " are correct") $ do
                  inputs <- readData ("test-data/" <> filename)
                  s <- flip execStateT freshStint $
                      forM inputs $ updateLapState . _dataGraphics
                  zipWithM_ shouldBe (s ^. finishedLaps) correctLaps
        in do
            lapTimeCheck "brands-hatch-3-laps.json" brandsHatch3Laptimes
            lapTimeCheck "zolder-5-laps-tyre-temps.json" zolder5Laptimes
    describe "Real-Life Stint Data" $ do
        it "Number of Stints is correct" $ do
            inputs <- readData "test-data/zolder-5-laps-tyre-temps.json"
            s <- flip execStateT freshSession $
                forM inputs $ \(DataPoint gp _ pp) -> updateStintState pp gp

            length (s ^. stints) `shouldBe` 2


main :: IO ()
main = hspec sessionStateSpec
