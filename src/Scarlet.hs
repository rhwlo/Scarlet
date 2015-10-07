{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Monad (join)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Data.Foldable (foldl')
import Data.Maybe (catMaybes)
import Data.Monoid (Monoid(..), (<>))
import Data.Text (Text)
import Data.Time.Clock
import Data.Time.Format
import Text.Pandoc (def, readMarkdown, writeHtmlString)
import Text.Pandoc.Error (handleError)
import Yesod
import Yesod.Static
import qualified Data.Text as T

import Scarlet.Entry

data Scarlet = Scarlet { getConnPool :: ConnectionPool, getStatic :: Static }

pageLength :: Int
pageLength = 2

blogTitle :: String
blogTitle = "Scarlet"

blogAuthor :: String
blogAuthor = "joshu"

instance Yesod Scarlet
instance YesodPersist Scarlet where
  type YesodPersistBackend Scarlet = SqlBackend
  runDB action = do
    Scarlet pool _ <- getYesod
    runSqlPool action pool

type ScarletHandler = HandlerT Scarlet IO

staticFiles "static"

mkYesod "Scarlet" [parseRoutes|
/                     AllR        GET
/from/#Int            StartFromR  GET
/just/#Int            JustR       GET
/next-after/#Int      NextAfterR  GET
/static StaticR Static getStatic
!/#String             SingleR     GET
|]

formatEntry :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
            => Entity Entry
            -> WidgetT Scarlet m ()
formatEntry (Entity _ entry) = let
  htmlContent = handleError (writeHtmlString def <$> readMarkdown def (entryContent entry))
    in [whamlet|
<div class=post id=t#{formatTime defaultTimeLocale "%s" (entryCtime entry)}>
 <h1 title=#{show $ entryCtime entry}>
  <a href=@{SingleR (entryUri entry)}>
   #{entryTitle entry}
 #{preEscapedToMarkup htmlContent}|]

formatDogear :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
             => Int
             -> WidgetT Scarlet m ()
formatDogear pageNumber = [whamlet|
<div class=anchor id=page#{show pageNumber}>
  <a href=@{StartFromR pageNumber}>⚓
|]

formatStub :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
           => Entity Entry
           -> WidgetT Scarlet m ()
formatStub (Entity _ entry) = [whamlet|
<div class=stub id=t#{formatTime defaultTimeLocale "%s" (entryCtime entry)}>
|]

intercalateWith :: Monoid m => Int -> (Int -> m) -> [m] -> m
intercalateWith divLen makeSeparator xs = foldl' accumXs mempty [0..(length xs `div` divLen - 1)]
  where
    accumXs x divNum = let
        remainingXs = drop (divLen * divNum) xs
      in
        x <> makeSeparator divNum <> mconcat (take divLen remainingXs)

formatWithDogears :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
                   => Int
                   -> [WidgetT Scarlet m ()]
                   -> WidgetT Scarlet m ()
formatWithDogears pageOffset formattedEntries = let
    dogearFormatter = formatDogear . (+ pageOffset)
  in
    intercalateWith pageLength dogearFormatter formattedEntries

template title body = [whamlet|
<html>
 <head>
  <link rel=stylesheet href=@{StaticR ship_css}>
  <script src=@{StaticR scarlet_js}>
  <title>#{title}
 <body>
  <div id=title>
   <h1>
    <a href=@{AllR}>#{blogTitle}
   <br>
   by #{blogAuthor}<br>
   (
    <a href=@{SingleR "about"}>learn more
   )
  <div id=main>
   ^{body}
|]

withoutLayout :: WidgetT Scarlet IO () -> ScarletHandler Html
withoutLayout = (>>= withUrlRenderer . pageBody) . widgetToPageContent

getJustR :: Int -> ScarletHandler Html
getJustR timestamp = let
    maybeTimeStamps :: [UTCTime]
    maybeTimeStamps = catMaybes $ parseTimeM True defaultTimeLocale "%s" . show <$> [timestamp, timestamp + 1]
  in case maybeTimeStamps of
    [someMinTime, someMaxTime] -> do
      results <- runDB $ selectFirst [EntryCtime >=. someMinTime, EntryCtime <=. someMaxTime] []
      case results of
        Just entry -> withoutLayout (formatEntry entry)
        Nothing -> withUrlRenderer $ [hamlet||]
    _ -> withUrlRenderer $ [hamlet||]

getSingleR :: String -> ScarletHandler Html
getSingleR uri = do
  results <- runDB $ selectFirst [EntryUri ==. uri] []
  case results of
    Just (Entity e entry) ->
      defaultLayout $ template (blogTitle ++ " - " ++ entryTitle entry) (formatEntry (Entity e entry))
    Nothing -> defaultLayout $ template blogTitle [whamlet|<p>No such post found, sorry!|]

getContentAfter :: (MonadIO m, MonadBaseControl IO m, MonadThrow m)
                => (WidgetT Scarlet m () -> ScarletHandler Html)
                -> (Entity Entry -> WidgetT Scarlet m ())
                -> Int
                -> ScarletHandler Html
getContentAfter display formatter pageOffset = do
    entries <- runDB $ selectList [] [Desc EntryCtime,
                                      LimitTo pageLength,
                                      OffsetBy (pageOffset * pageLength)]
    display $ formatWithDogears pageOffset (formatter <$> entries)


getStartFromR :: Int -> ScarletHandler Html
getStartFromR = getContentAfter (defaultLayout . template blogTitle) formatEntry

getAllR :: ScarletHandler Html
getAllR = getStartFromR 0

getNextAfterR :: Int -> ScarletHandler Html
getNextAfterR = getContentAfter withoutLayout formatStub

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "scarlet.sqlite"
    openConnectionCount $ \pool -> liftIO $ do
      static@(Static settings) <- static "static"
      warp 3000 $ Scarlet pool static
  where
    openConnectionCount = 10
