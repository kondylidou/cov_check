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
{--for table
import Yesod.Colonnade
import Colonnade (Colonnade,Headed,Headless,headed,cap,Fascia(..))
import Text.Blaze.Html (Html, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as HA
--}
--for ugly programming by me
--import System.IO.Unsafe (unsafePerformIO)

getTestColonnadeAPIR :: Handler Html
getTestColonnadeAPIR = do
--    liftIO $ refreshDB
    defaultLayout $ do
        setTitle "Colonnade Testing"
        $(widgetFile "testColonnadeAPI")
{--
tablePreset :: Colonnade Headed Coviddata (WidgetFor site ())
tablePreset = mconcat
    [   headed "Country" (coviddataCountry)
    ,   headed "Cases" (coviddataCases)
    ,   headed "Deaths" (coviddataDeaths)
    ,   headed "Recoveries" (coviddataRecovered)
    ]

attrs = [HA.class_ "stylish-table"]

colonnadeTable list = encodeWidgetTable attrs tablePreset list --}
-- Here I want to get the data from the API into the Database
covidDataURL :: String
covidDataURL = "https://covid19-api.org/api/status"

countryDataURL :: String
countryDataURL = "https://covid19-api.org/api/countries"

allCovidDataJSON :: IO B.ByteString
allCovidDataJSON = simpleHttp $ covidDataURL

allCountryDataJSON :: IO B.ByteString
allCountryDataJSON = simpleHttp $ countryDataURL

refreshDB :: IO()
refreshDB = do
    cov <- (eitherDecode <$> allCovidDataJSON) :: IO (Either String [Coviddata])
    countries <- (eitherDecode <$> allCountryDataJSON) :: IO (Either String [Country])

    case cov of
        Left err -> print err
        Right d ->  refreshCoviddata d

    case countries of
        Left err -> print err
        Right d ->  refreshCountries d

refreshCoviddata :: [Coviddata] -> IO()
refreshCoviddata covject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CoviddataCases >=. 0]
    ids <- forM covject insert
    print ids

{--
testSelect :: IO()
testSelect = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    selection <- selectList [CoviddataCountry ==. "AF"] []
    print selection
--}
refreshCountries :: [Country] -> IO()
refreshCountries countject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CountryNumeric >=. ""]
    ids <- forM countject insert
    print ids
{--
testSelect' :: IO()
testSelect' = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    selection <- selectList [CountryName ==. "Afghanistan"] []
    print selection
--}