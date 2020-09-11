{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.TestYesodTableAPI where


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

getTestAPIR :: Handler Html
getTestAPIR = unsafePerformIO testReader

covidTable :: Table App Coviddata
covidTable = mempty
    <> Table.text "Country"     coviddataCountry
    <> Table.int "Cases"        coviddataCases
    <> Table.int "Deaths"       coviddataDeaths
    <> Table.int "Recoveries"   coviddataRecovered

testReader :: IO(HandlerFor App Html)
testReader = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Desc CoviddataCases]
    return $ testHandler $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

resetDB :: IO()
resetDB = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CoviddataCases >=. 0]
    deleteWhere [CountryNumeric >=. ""]
    return ()

testHandler c = defaultLayout $ Table.buildBootstrap covidTable c
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
        Right d ->  testInsert d
    
    testSelect

    case countries of
        Left err -> print err
        Right d ->  testInsert' d
    
    testSelect'

testInsert :: [Coviddata] -> IO()
testInsert covject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    ids <- forM covject insert
    print ids

testSelect :: IO()
testSelect = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    selection <- selectList [CoviddataCountry ==. "AF"] []
    print selection

testInsert' :: [Country] -> IO()
testInsert' countject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    ids <- forM countject insert
    print ids

testSelect' :: IO()
testSelect' = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    selection <- selectList [CountryName ==. "Afghanistan"] []
    print selection