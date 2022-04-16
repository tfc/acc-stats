{-# LANGUAGE CPP #-}

module Main where

import           Acc.Stats.API
import           Acc.Stats.Client
import           Acc.StatsPage
import           Acc.StatsPage.Binary
import           Control.Concurrent       (threadDelay)
import           Control.Exception        (bracket)
import           Control.Monad            (forever, (>=>))
import           Data.ByteString          (ByteString)
import qualified Data.ByteString.Lazy     as BL
import qualified Data.ByteString.Unsafe   as BSU
import           Foreign.Ptr              (Ptr)
import           GHC.Ptr                  (Ptr, plusPtr)
import           Network.HTTP.Client      hiding (Proxy)
import           Network.HTTP.Types
import           Servant.API
import           Servant.Client
import           System.Win32.FileMapping

acpmfPhysics = "Local\\acpmf_physics"
acpmfStatic = "Local\\acpmf_static"
acpmfGraphics = "Local\\acpmf_graphics"

withSharedWindowsMapping :: String -- shared mem fs path
                         -> Int    -- Size in bytes
                         -> ((Ptr b, Int) -> IO a)
                         -> IO a
withSharedWindowsMapping path bytes f = let
        openPtr = do
            fm <- openFileMapping fILE_MAP_ALL_ACCESS False (Just path)
            ptr <- mapViewOfFile fm fILE_MAP_ALL_ACCESS 0 (fromIntegral bytes)
            return (ptr, bytes)
    in bracket
        openPtr
        (unmapViewOfFile . fst)
        f

main :: IO ()
main = do
    pd <- getFunctionPostDataPoint
    withSharedWindowsMapping acpmfPhysics (structureSize getPhysicsPage) $ \pagePhys ->
        withSharedWindowsMapping acpmfStatic (structureSize getStatPage) $ \pageStat ->
            withSharedWindowsMapping acpmfGraphics (structureSize getGraphicsPage) $ \pageGraph ->
                forever $ do
                    let f a b = readStructure a <$> BSU.unsafePackCStringLen b
                    pp <- f getPhysicsPage pagePhys
                    ps <- f getStatPage pageStat
                    pg <- f getGraphicsPage pageGraph
                    print $ _physicsPageGear pp
                    print $ _physicsPageWheelsPressure pp
                    print =<< pd (DataPoint pg ps pp)
                    threadDelay 3000000
