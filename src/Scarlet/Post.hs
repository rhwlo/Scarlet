{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Scarlet.Post.Internal ((<$$>), EntryError(..), EntryResult(..))
import qualified Scarlet.Post.Internal as SPI
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO, liftIO)
import qualified Data.Text as T
import qualified Data.ByteString.Internal as BSI
import Database.Persist.Sqlite
import Network.HTTP (RequestMethod(PUT), headRequest, mkRequest, setRequestBody, simpleHTTP)
import Network.Mime (defaultMimeLookup)
import Network.URI (parseURI)
import qualified Scarlet.Entry as SE
import System.Environment (getArgs, getEnv)
import System.IO (Handle(..), hGetContents, openFile)
import Text.Printf

parseEntryFromFile :: Handle          -- the handle for the file to read from
                   -> IO (EntryResult SE.Entry)
parseEntryFromFile fileHandle = SPI.parseEntry =<< hGetContents fileHandle

handleUris :: SE.Entry        -- whatever entry exists thus far
           -> IO ()
handleUris entry = do
    results <- SPI.handleAbsentUris getHandler putHandler entry
    forM_ results $ \result ->
      case result of
        Left someError -> print someError
        _              -> return ()
  where
    getHandler = simpleHTTP . headRequest
    putHandler :: String -> String -> IO (EntryResult ())
    putHandler localCopy remoteUri =
      case parseURI remoteUri of
        Nothing -> return $ Left $ AssetError ("PUT request: not a valid URL: " ++ remoteUri)
        Just u  -> let
            contentType = BSI.unpackChars $ defaultMimeLookup (T.pack localCopy)
          in do
            fileBody <- readFile localCopy
            httpPutResult <- simpleHTTP (setRequestBody (mkRequest PUT u) (contentType, fileBody))
            case httpPutResult of
              Left someConnError -> return (Left (AssetNetworkError someConnError))
              Right _            -> return (Right ())

main :: IO ()
main = runSqlite "scarlet.sqlite" $ do
  runMigration $ migrate SE.entityDefs $ entityDef (Nothing :: Maybe SE.Entry)
  eitherEntry <- liftIO $ do
    fileContents <- readFile =<< (head <$> getArgs)
    SPI.parseEntry fileContents
  case eitherEntry of
    Left errorString -> liftIO $ print errorString
    Right entry      -> do
      entryId <- insert entry
      liftIO $ printf "Added your entry \"%s\"\n" (SE.entryTitle entry)
