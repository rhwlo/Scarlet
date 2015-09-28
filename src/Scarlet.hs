{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Monad.Logger (runStderrLoggingT)
import Control.Monad.Trans.Resource (runResourceT)
import Database.Persist.Sqlite
import Data.Text (Text)
import Text.Pandoc (def, readMarkdown, writeHtmlString)
import Text.Pandoc.Error (handleError)
import Yesod
import qualified Data.Text as T

import Scarlet.Entry

data Scarlet = Scarlet ConnectionPool
instance Yesod Scarlet
instance YesodPersist Scarlet where
  type YesodPersistBackend Scarlet = SqlBackend
  runDB action = do
    Scarlet pool <- getYesod
    runSqlPool action pool

type ScarletHandler = HandlerT Scarlet IO

mkYesod "Scarlet" [parseRoutes|
/                  RootR       GET
/just/#Integer     JustR       GET
/#String           SingleR     GET
|]

getRootR :: ScarletHandler Html
getRootR = defaultLayout [whamlet|<h1>Hello, world|]

getJustR :: Integer -> ScarletHandler Html
getJustR aNumber = defaultLayout [whamlet|<h1>This is #{aNumber}|]

getSingleR :: String -> ScarletHandler Html
getSingleR uri = do
  results <- runDB $ selectFirst [EntryUri ==. uri] []
  case results of
    Just (Entity _ entry) -> let
        htmlContent = handleError (writeHtmlString def <$> readMarkdown def (entryContent entry))
      in defaultLayout [whamlet|
<html>
  <head>
    <title>Scarlet: #{entryTitle entry}
  <body>
    <h1>#{entryTitle entry}
    #{preEscapedToMarkup htmlContent}|]
    Nothing -> defaultLayout [whamlet|
<html>
  <head>
    <title>No such post found</title>
  <body>
    <h1>No such post NotFound
    <p>Sorry!|]

getAllR :: ScarletHandler String
getAllR = do
  entries <- runDB $ selectList [EntryUri !=. ""] []
  return $ show entries

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "scarlet.sqlite"
    openConnectionCount $ \pool -> liftIO $
      warp 3000 $ Scarlet pool
  where
    openConnectionCount = 10
