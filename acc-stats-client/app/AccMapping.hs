module AccMapping where

import           Acc.StatsPage
import           Acc.StatsPage.Binary
import           Control.Exception        (bracket)
import qualified Data.ByteString.Unsafe   as BSU
import           Foreign.C.Types          (CChar)
import           Foreign.Ptr              (Ptr)
import           System.Win32.FileMapping

acpmfPhysics :: String
acpmfPhysics = "Local\\acpmf_physics"
acpmfStatic :: String
acpmfStatic = "Local\\acpmf_static"
acpmfGraphics :: String
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

type StatsPagePointers a = ((Ptr a, Int), (Ptr a, Int), (Ptr a, Int))

withMappings :: (IO FullData -> IO a)
             -> IO a
withMappings f =
    withSharedWindowsMapping acpmfPhysics (structureSize getPhysicsPage) $ \pagePhys ->
        withSharedWindowsMapping acpmfStatic (structureSize getStatPage) $ \pageStat ->
            withSharedWindowsMapping acpmfGraphics (structureSize getGraphicsPage) $ \pageGraph ->
                f $ getFullDataFunction (pageGraph, pagePhys, pageStat)
    where
        readFromMem a b = readStructure a <$> BSU.unsafePackCStringLen b
        getFullDataFunction :: StatsPagePointers CChar -> IO FullData
        getFullDataFunction (pageGraph, pagePhys, pageStat) =
            FullData <$> readFromMem getGraphicsPage pageGraph
                     <*> readFromMem getPhysicsPage pagePhys
                     <*> readFromMem getStatPage pageStat
