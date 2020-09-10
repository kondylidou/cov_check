{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.TestAPI where

import Import
import Text.Julius()
--for parsing
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
--for database
import Database.Persist
import Database.Persist.Sqlite
import Database.Persist.TH


getTestAPIR :: Handler Html
getTestAPIR = do
    defaultLayout $ do
        setTitle "API Testpage"
        $(widgetFile "testAPI")
--        liftIO $ testRun

-- Here I want to get the data from the API into the Database
covidDataURL :: String
covidDataURL = "https://covid19-api.org/api/status"

countryDataURL :: String
countryDataURL = "https://covid19-api.org/api/countries"

allCovidDataJSON :: IO B.ByteString
allCovidDataJSON = simpleHttp $ covidDataURL

allCountryDataJSON :: IO B.ByteString
allCountryDataJSON = simpleHttp $ countryDataURL

testRun :: IO()
testRun = do
    cov <- (eitherDecode <$> allCovidDataJSON) :: IO (Either String [Coviddata])
    countries <- (eitherDecode <$> allCountryDataJSON) :: IO (Either String [Country])

    case cov of
        Left err -> print err
        Right d -> testInsert d

    case countries of
        Left err -> print err
        Right d -> testInsert' d

testInsert :: [Coviddata] -> IO()
testInsert covject = runSqlite ":memory:" $ do
    runMigration migrateAll

    ids <- forM covject insert

    testSelect <- selectList [CoviddataCountry ==. "AF"] []
    print testSelect

testInsert' :: [Country] -> IO()
testInsert' countject = runSqlite ":memory:" $ do
    runMigration migrateAll

    ids <- forM countject insert

    testSelect <- selectList [CountryName ==. "Afghanistan"] []
    print testSelect