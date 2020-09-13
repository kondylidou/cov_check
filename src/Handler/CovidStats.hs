{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.CovidStats where


import Import
import Text.Julius()
--for parsing
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson
import qualified Data.ByteString.Lazy.Internal as B
--for database
import Database.Persist.Sqlite
--for table
import Yesod.Table (Table)
import qualified Yesod.Table as Table
import System.IO.Unsafe (unsafePerformIO)

getCovidStatsR :: Handler Html
getCovidStatsR = do
        defaultLayout $ do
            setTitle "Global Covid-19 Statistics"
            $(widgetFile "covidStats")


buildTableCov :: Table site Coviddata -> [Coviddata] -> WidgetT site IO ()
buildTableCov (Table.Table cols) vals = table $ do
  thead $ mapM_ Table.header cols
  tbody $ forM_ vals $ \val -> tr $ forM_ cols $ \col -> Table.cell col val
  where table b  = asWidgetIO [whamlet|
                     <table class="table table-striped table-hover" id="covtable">^{b}
                   |]
        thead b  = asWidgetIO [whamlet|
                     <thead style="border: 1px solid black;">
                       <tr>
                         ^{b}
                   |]
        tbody b  = asWidgetIO [whamlet|
                     <tbody>^{b}
                   |]
        tr b     = asWidgetIO [whamlet|
                     <tr style="border: 1px solid black;">^{b}
                   |]

buildTableCountry :: Table site Country -> [Country] -> WidgetT site IO ()
buildTableCountry (Table.Table cols) vals = table $ do
  thead $ mapM_ Table.header cols
  tbody $ forM_ vals $ \val -> tr $ forM_ cols $ \col -> Table.cell col val
  where table b  = asWidgetIO [whamlet|
                     <table class="table table-striped table-hover" id="countrytable">^{b}
                   |]
        thead b  = asWidgetIO [whamlet|
                     <thead style="border: 1px solid black;">
                       <tr>
                         ^{b}
                   |]
        tbody b  = asWidgetIO [whamlet|
                     <tbody>^{b}
                   |]
        tr b     = asWidgetIO [whamlet|
                     <tr style="border: 1px solid black;">^{b}
                   |]

asWidgetIO :: WidgetT site IO () -> WidgetT site IO ()
asWidgetIO = id

covidTable :: Table App Coviddata
covidTable = mempty
    <> Table.text "Country"     coviddataCountry
    <> Table.int "Cases"        coviddataCases
    <> Table.int "Deaths"       coviddataDeaths
    <> Table.int "Recoveries"   coviddataRecovered

countryTable :: Table App Country
countryTable = mempty
    <> Table.text "Abbreviation" countryAlpha2
    <> Table.text "Country"     countryName    

countriesAsc :: IO([Country])
countriesAsc = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Asc CountryAlpha2]
    return $ fmap (\Entity {entityKey=_, entityVal=b} -> b) countries

casesDesc :: IO([Coviddata])
casesDesc = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Desc CoviddataCases]
    return $ fmap (\Entity {entityKey=_, entityVal=b} -> b) countries

tableGeneralized :: (PersistEntity b, PersistEntityBackend b ~ SqlBackend) => 
                    (a -> SelectOpt b) -> a -> IO([b])
tableGeneralized orientation content = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [(orientation) (content)]
    return $  fmap (\Entity {entityKey=_, entityVal=b} -> b) countries

resetDB :: IO()
resetDB = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CoviddataCases >=. 0]
    deleteWhere [CountryNumeric >=. ""]
    return ()

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
    _ <- forM covject insert
    return ()

refreshCountries :: [Country] -> IO()
refreshCountries countject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CountryNumeric >=. ""]
    _ <- forM countject insert
    return ()