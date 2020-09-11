{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.TestColonnadeAPI where

import Import
import Text.Julius()
import Model (Coviddata)
--for parsing
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
--for database
import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
--for table
import Yesod.Table (Table)
import qualified Yesod.Table as Table
--for ugly programming by me
import System.IO.Unsafe (unsafePerformIO)

getTestColonnadeAPIR :: Handler Html
getTestColonnadeAPIR = do
    defaultLayout $ do
        setTitle "Colonnade Testing"
        $(widgetFile "testColonnadeAPI")