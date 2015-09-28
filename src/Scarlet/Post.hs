{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts  #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (liftIO)
import Data.List (intercalate)
import qualified Data.Map as DM
import Database.Persist.Sqlite
import Data.Maybe (fromMaybe, maybe)
import Data.Time.Clock
import Data.Time.Format
import qualified Scarlet.Entry as SE
import System.Environment (getArgs, getEnv)
import Text.ParserCombinators.Parsec
import Text.Printf

data ParsedEntry = ParsedEntry (Maybe String)     -- ctime:   optional (defaults to now)
                               (Maybe String)     -- uri:     optional (defaults to title-derived)
                               String             -- title
                               String             -- content
                               (Maybe String)     -- byline:  optional (defaults to $USER)
                               String             -- language

entryFromParsedEntry :: Either ParseError ParsedEntry -> IO (Either ParseError SE.Entry)
entryFromParsedEntry (Right (ParsedEntry mCtime mUri title content mByline language)) = let
    iCtime :: IO UTCTime
    iCtime = maybe getCurrentTime return (mCtime >>= parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z")
    uri :: String
    uri = fromMaybe (intercalate "," (words title)) mUri
    iByline :: IO String
    iByline = maybe (getEnv "USER") return mByline
  in
    iByline >>= \byline ->
      iCtime >>= \ctime ->
        return $ Right $ SE.Entry ctime uri title content byline language
entryFromParsedEntry (Left parseError) = return (Left parseError)

directive    :: CharParser st (String, String)
directive    = do
    string "<!--"
    keyword <- manyTill anyChar (string ": ")
    value <- manyTill anyChar (string "-->")
    return (keyword, value)

entryParser  :: CharParser st ParsedEntry
entryParser = do
    directives <- DM.fromList <$> many directiveLine
    content <- many anyChar
    return $ ParsedEntry (DM.lookup "date" directives)
                         (DM.lookup "uri"  directives)
                         (directives DM.! "title")
                         content
                         (DM.lookup "by" directives)
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
        makeUriFromTitle = intercalate "-" . words
      in fromMaybe (makeUriFromTitle title) mUri
    defaultedByline :: Maybe String -> IO String
    defaultedByline = maybe (getEnv "USER") return

main :: IO ()
main = runSqlite "scarlet.sqlite" $ do
  runMigration $ migrate SE.entityDefs $ entityDef (Nothing :: Maybe SE.Entry)
  eitherEntry <- liftIO $ do
    (filename:_) <- getArgs
    parseFromFile entryParser filename >>= entryFromParsedEntry
  case eitherEntry of
    Left errorString -> liftIO $ print errorString
    Right entry      -> do
      entryId <- insert entry
      liftIO $ printf "Added your entry \"%s\"\n" (SE.entryTitle entry)
