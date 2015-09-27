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
/all               AllR        GET
|]

getRootR :: ScarletHandler Html
getRootR = defaultLayout [whamlet|<h1>Hello, world|]

getJustR :: Integer -> ScarletHandler Html
getJustR aNumber = defaultLayout [whamlet|<h1>This is #{aNumber}|]

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
