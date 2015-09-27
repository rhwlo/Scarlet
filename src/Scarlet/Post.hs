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

data FieldName = Byline
               | Ctime
               | Language
               | Title
               | Uri
             deriving (Eq, Ord, Read, Show)

entryFromParams :: Maybe String     -- ctime, defaulting to now
                -> Maybe String     -- uri, defaulting to derived from title
                -> String           -- title
                -> String           -- content
                -> Maybe String     -- byline, defaulting to $USER
                -> String           -- language
                -> IO SE.Entry         -- the complete entry
entryFromParams mCtime mUri title content mByline language = let
    iCtime :: IO UTCTime
    iCtime = maybe getCurrentTime return (mCtime >>= parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z")
    uri :: String
    uri = fromMaybe (intercalate "," (words title)) mUri
    iByline :: IO String
    iByline = maybe (getEnv "USER") return mByline
  in
    iByline >>= \byline ->
      iCtime >>= \ctime ->
        return $ SE.Entry ctime uri title content byline language

directive    :: CharParser st (FieldName, String)
directive    = comment $ do
    keyword <- manyTill anyChar (string ": ")
    value <- quotedString
    return (read keyword, value)
  where
    comment = between (string "<!--") (string "-->")
    quotedString = string "\"" >> manyTill anyChar (string "\"")

entryParser  :: CharParser st (IO SE.Entry)
entryParser = do
    directives <- DM.fromList <$> many directiveLine
    content <- many anyChar
    return $ entryFromParams (DM.lookup Ctime directives)
                             (DM.lookup Uri directives)
                             (directives DM.! Title)
                             content
                             (DM.lookup Byline directives)
                             (directives DM.! Language)
  where
    directiveLine = do
      d <- directive
      newline
      return d

main :: IO ()
main = runSqlite "scarlet.sqlite" $ do
  runMigration $ migrate SE.entityDefs $ entityDef (Nothing :: Maybe SE.Entry)
  (filename:_) <- liftIO getArgs
  eitherEntry <- liftIO (parseEntryFromFile filename)
  case eitherEntry of
    Left errorString -> liftIO $ print errorString
    Right entry      -> do
      entryId <- insert entry
      liftIO $ printf "Added your entry \"%s\"\n" (SE.entryTitle entry)

parseEntryFromFile :: String -> IO (Either String SE.Entry)
parseEntryFromFile filename = do
  parsedEntry <- parseFromFile entryParser filename
  case parsedEntry of
    Left errorString        -> return $ Left (show errorString)
    Right entry             -> Right <$> entry
