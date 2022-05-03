{-# LANGUAGE TypeOperators #-}

module Main where

import           Acc.Stats.Server
import           Network.Wai.Handler.Warp
import           Options.Applicative

data Config = Config
  { configPort :: Int
  , configStaticPath :: Maybe String
  }

configParser :: Parser Config
configParser = Config
      <$> option auto
          ( long "port"
         <> help "TCP Port to serve on"
         <> showDefault
         <> value 8000
         <> metavar "PORT" )
      <*> optional (strOption
          ( long "static-path"
         <> metavar "STATIC PATH"
         <> help "Path for static content like client zip file and web UI. Omitting disables CORS."
          ))

opts :: ParserInfo Config
opts = info (configParser <**> helper)
    ( fullDesc
    <> progDesc "Print a greeting for TARGET"
    <> header "hello - a test for optparse-applicative" )

main :: IO ()
main = do
    config <- execParser opts
    putStrLn $ "Serving on port " <> show (configPort config)
    run (configPort config) serverApp
