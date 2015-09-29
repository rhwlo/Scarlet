{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (toLower)
import Data.List (intercalate)
import qualified Data.Map as DM
import Data.Maybe (fromMaybe, maybe)
import Data.Time.Clock
import Data.Time.Format
import Database.Persist.Sqlite
import qualified Scarlet.Entry as SE
import System.Environment (getArgs, getEnv)
import Text.Parsec
import Text.Printf

directive :: ParsecT String () IO (String, String)
directive = do
    string "<!--"                               -- the comment starts
    key <- quotedOrNotUntil (string ": ")       -- "key": or key:
    value <- quotedOrNotUntil (string "-->")    -- "value" or value
    return (key, value)
  where
    quotedUntil s = do
      openingQuote <- string "\"" <|> string "'"
      middleStuff <- manyTill anyChar (string openingQuote)
      s
      return middleStuff
    quotedOrNotUntil s = quotedUntil s <|> manyTill anyChar s

entryParser :: ParsecT String () IO SE.Entry
entryParser = do
    directives <- DM.fromList <$> many directiveLine
    content <- many anyChar
    ctime <- liftIO $ defaultedCtime (DM.lookup "date" directives)
    byline <- liftIO $ defaultedByline (DM.lookup "by" directives)
    return $ SE.Entry ctime
                      (defaultedUri directives)
                      (directives DM.! "title")
                      content
                      byline
                      (directives DM.! "lang")
  where
    directiveLine = do
      d <- directive
      newline
      return d
    defaultedCtime :: Maybe String -> IO UTCTime
    defaultedCtime mCtimeString = maybe getCurrentTime return $
      mCtimeString >>= parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z"
    defaultedUri   :: DM.Map String String -> String
    defaultedUri directives = let
        title :: String
        title = directives DM.! "title"
        mUri  :: Maybe String
        mUri  = DM.lookup "uri" directives
        makeUriFromTitle :: String -> String
        makeUriFromTitle = intercalate "-" . words . map toLower
      in fromMaybe (makeUriFromTitle title) mUri
    defaultedByline :: Maybe String -> IO String
    defaultedByline = maybe (getEnv "USER") return

main :: IO ()
main = runSqlite "scarlet.sqlite" $ do
  runMigration $ migrate SE.entityDefs $ entityDef (Nothing :: Maybe SE.Entry)
  eitherEntry <- liftIO $ do
    fileContents <- readFile =<< (head <$> getArgs)
    runParserT entryParser () "" fileContents
  case eitherEntry of
    Left errorString -> liftIO $ print errorString
    Right entry      -> do
      entryId <- insert entry
      liftIO $ printf "Added your entry \"%s\"\n" (SE.entryTitle entry)
