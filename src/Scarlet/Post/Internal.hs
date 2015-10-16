{-# LANGUAGE OverloadedLists,
             FlexibleContexts,
             ViewPatterns       #-}

module Scarlet.Post.Internal
where

import Control.Monad (filterM)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Char (isAlphaNum, toLower)
import Data.List (foldl', intercalate)
import qualified Data.Map as DM
import Data.Maybe (fromMaybe, maybe)
import Data.Time.Clock
import Data.Time.Format
import Database.Persist.Sqlite
import Network.HTTP (HStream, Request(..), Response(..), headRequest, simpleHTTP)
import Network.URI (isAbsoluteURI)
import qualified Network.Stream
import qualified Scarlet.Entry as SE
import System.Environment (getArgs, getEnv)
import Text.Parsec
import Text.Pandoc
import Text.Pandoc.Error (handleError)
import Text.Printf

class Pandocoid p where
  toPandoc    :: p -> Pandoc

class Entryoid e where
  directives  :: e -> DM.Map String String
  entryUri    :: e -> String

instance Pandocoid Pandoc where
  toPandoc = id

instance Entryoid Pandoc where
  directives _ = []
  entryUri _ = ""

instance Pandocoid SE.Entry where
  toPandoc = handleError . readMarkdown def . SE.entryContent

instance Entryoid SE.Entry where
  directives = read . SE.entryDirectives
  entryUri = SE.entryUri

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

scanForAssets :: Pandocoid p => p -- takes a Pandoc document
              -> [String]         -- returns a list of asset URIs as strings
scanForAssets (toPandoc -> Pandoc _ blocks) = scanBlockForAssets =<< blocks
  where
    scanBlockForAssets :: Block -> [String]
    scanBlockForAssets (Plain inlines)  = inlines >>= getAssets
    scanBlockForAssets (Para inlines)   = inlines >>= getAssets
    scanBlockForAssets (Div _ divBlocks)  = divBlocks >>= scanBlockForAssets
    scanBlockForAssets _                  = []
    getAssets :: Inline -> [String]
    getAssets (Image _ (uri, _))          = [uri]
    getAssets (Link _ (uri, _))           = [uri]
    getAssets _                           = []

scanForRelativeURIs :: (Entryoid p, Pandocoid p) => p   -- takes a Pandoc document
                    -> [String]                         -- returns a list of relative URIs
scanForRelativeURIs = filter (not . isAbsoluteURI) . scanForAssets

scanForAbsentRelativeUris :: (Entryoid p, Pandocoid p) => p  -- takes a Pandoc document
                          -> IO [String]                     -- returns a (side-effecty) list of relative URIs
scanForAbsentRelativeUris = scanForAbsentRelativeUrisWithHTTP (simpleHTTP . headRequest)

scanForAbsentRelativeUrisWithHTTP :: (Entryoid entripandocoid, Pandocoid entripandocoid)
                                  => (String -> IO (Network.Stream.Result (Response String)))
                                  -- an HTTP handler
                                  -> entripandocoid   -- the document to process
                                  -> IO [String]      -- the list of absent relative URIs
scanForAbsentRelativeUrisWithHTTP httpHandler doc = let
    relativeUris :: [String]
    relativeUris = scanForRelativeURIs doc
    isAbsent :: String -> IO Bool
    isAbsent uri = do
        resp <- httpHandler uri
        return $ case resp of
          Right (Response { rspCode = (2, 0, _) }) -> False
          Right (Response { rspCode = (3, 0, _) }) -> False
          _                                        -> True
      where
        qualifiedUri :: String
        qualifiedUri = let
            directivesMap :: DM.Map String String
            directivesMap = directives doc
            maybeStaticHost :: Maybe String
            maybeStaticHost = DM.lookup "static_host" directivesMap
            maybePrefix :: Maybe String
            maybePrefix = do
              staticHost <- maybeStaticHost
              return $ intercalate "/" ["http:/", staticHost, entryUri doc]
          in
            fromMaybe "" maybePrefix ++ uri
  in
    filterM isAbsent relativeUris

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
                      (show $ otherDirectivesFrom directives)
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
        makeUriFromTitle = intercalate "-" . map (filter isAlphaNum) . words . map toLower
      in fromMaybe (makeUriFromTitle title) mUri
    defaultedByline :: Maybe String -> IO String
    defaultedByline = maybe (getEnv "USER") return
    consumedDirectives :: [String]
    consumedDirectives = ["by", "date", "lang", "title", "uri"]
    otherDirectivesFrom :: DM.Map String String -> DM.Map String String
    otherDirectivesFrom = flip (foldl' (flip DM.delete)) consumedDirectives

parseEntry :: String -> IO (Either ParseError SE.Entry)
parseEntry = runParserT entryParser () ""
