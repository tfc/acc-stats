{-# LANGUAGE TypeOperators #-}

module Main where

import           Acc.Stats.API
import           Acc.Stats.Server
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Cors
import           Options.Applicative
import           Servant

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

app :: TC -> Application
app = simpleCors . serve accStatsApiProxy . server

type ApiAndStatic = AccStatsApi :<|> Raw

main :: IO ()
main = do
    config <- execParser opts
    putStrLn $ "Serving on port " <> show (configPort config)
    tc <- emptyServerState
    case configStaticPath config of
        Nothing -> run (configPort config) $ simpleCors $ serve accStatsApiProxy $ server tc
        Just p -> run (configPort config) $ simpleCors $ serve (Proxy :: Proxy ApiAndStatic) (server tc :<|> serveDirectoryFileServer p)
