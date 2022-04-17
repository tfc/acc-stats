{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import Data.Traversable (for)
import           Hasql.Connection
import           Hasql.Migration
import           Hasql.Session              (QueryError, run)
import qualified Hasql.Transaction          as Tx
import qualified Hasql.Transaction.Sessions as Tx
import qualified Data.ByteString.Char8 as BS

runTx :: Connection -> Tx.Transaction a -> IO (Either QueryError a)
runTx con act = do run (Tx.transaction Tx.ReadCommitted Tx.Write act) con

main :: IO ()
main = acquire "" >>= \case
    Right con -> do
        runTx con (Tx.sql "DROP OWNED BY tfc;")
        runTx con (runMigration $ MigrationInitialization) >>= print
        migrationScripts <- loadMigrationsFromDirectory "migration-scripts"
        for migrationScripts $ \m@(MigrationScript name script) -> do
            putStrLn $ "====================== " <> name
            BS.putStrLn script
            runTx con (runMigration m) >>= \case
                Right Nothing -> putStrLn "Success"
                x -> print x
        return ()
    Left err -> print err
