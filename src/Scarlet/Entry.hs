{-# LANGUAGE  EmptyDataDecls,
              FlexibleContexts,
              GADTs,
              GeneralizedNewtypeDeriving,
              MultiParamTypeClasses,
              OverloadedStrings,
              QuasiQuotes,
              TemplateHaskell,
              TypeFamilies                  #-}

module Scarlet.Entry where

import Control.Monad.IO.Class (liftIO)
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH
import Data.Time.Clock (UTCTime)

share [mkPersist sqlSettings, mkSave "entityDefs"]
  [persistLowerCase|
  Entry
      ctime       UTCTime
      uri         String
      title       String
      content     String
      byline      String
      language    String
      directives  String
    deriving Show
  |]
