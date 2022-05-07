{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Acc.Stats.Migrations where

import           Control.Monad              (forM_)
import qualified Data.ByteString.Char8      as BS
import           Hasql.Connection
import           Hasql.Migration
import           Hasql.Session              (QueryError, run)
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as Tx

runTx :: Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con = flip run con . Tx.transaction Tx.ReadCommitted Tx.Write

runMigrations :: Connection -> IO ()
runMigrations con = do
    _ <- runTx con (Tx.sql "DROP OWNED BY tfc;")
    runTx con (runMigration $ MigrationInitialization) >>= print
    migrationScripts <- loadMigrationsFromDirectory "migration-scripts"
    forM_ migrationScripts $ \m@(MigrationScript name script) -> do
        putStrLn $ "====================== " <> name
        BS.putStrLn script
        runTx con (runMigration m) >>= \case
            Right Nothing -> putStrLn "Success"
            x -> print x
