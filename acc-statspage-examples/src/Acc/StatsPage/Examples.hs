{-# LANGUAGE TemplateHaskell #-}
module Acc.StatsPage.Examples where

import           Acc.StatsPage
import           Acc.StatsPage.Binary
import           Data.ByteString      (ByteString)
import           Data.FileEmbed       (embedFile)

graphicsPageRaw :: ByteString
graphicsPageRaw = $(embedFile "examples/graphics.txt")

statPageRaw :: ByteString
statPageRaw = $(embedFile "examples/stats.txt")

physicsPageRaw :: ByteString
physicsPageRaw = $(embedFile "examples/physics.txt")

graphicsPage :: GraphicsPage
graphicsPage = readStructure getGraphicsPage graphicsPageRaw

statPage :: StatPage
statPage = readStructure getStatPage statPageRaw

physicsPage :: PhysicsPage
physicsPage = readStructure getPhysicsPage physicsPageRaw
