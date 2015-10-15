{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Scarlet.Post.Internal (parseEntry)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Database.Persist.Sqlite
import qualified Scarlet.Entry as SE
import System.Environment (getArgs, getEnv)
import Text.Printf

main :: IO ()
main = runSqlite "scarlet.sqlite" $ do
  runMigration $ migrate SE.entityDefs $ entityDef (Nothing :: Maybe SE.Entry)
  eitherEntry <- liftIO $ do
    fileContents <- readFile =<< (head <$> getArgs)
    parseEntry fileContents
  case eitherEntry of
    Left errorString -> liftIO $ print errorString
    Right entry      -> do
      entryId <- insert entry
      liftIO $ printf "Added your entry \"%s\"\n" (SE.entryTitle entry)
