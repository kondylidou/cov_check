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

data Global = Global 
        { newConfirmed :: Int
        , totalConfirmed :: Int
        , newDeaths :: Int
        , totalDeaths :: Int
        , newRecovered :: Int
        , totalRecovered :: Int
        }
        deriving (Show, Generic)

instance ToJSON Global
instance FromJSON Global

getTestYesodTableAPIR :: Handler Html
getTestYesodTableAPIR = do
    liftIO $ refreshDB
    defaultLayout $ do
        setTitle "Global Covid-19 Statistics"
        [whamlet|
<div .container>
    <div .jumbotron>
        <h2> <center> Worldwide Stats about the Coronavirus
        <div style="height:400px;overflow:auto;" .jumbotron>
            ^{Table.buildBootstrap covidTable $ unsafePerformIO casesDesc}
        <div style="height:350px;overflow:auto;" .jumbotron>
            ^{Table.buildBootstrap countryTable $ unsafePerformIO countriesAsc}
        Source: <a href=https://covid19-api.org/> Covid-19 API </a> 
        |]
--        Table.buildBootstrap covidTable $ unsafePerformIO casesDesc
--    unsafePerformIO $ tableCasesDesc

covidTable :: Table App Coviddata
covidTable = mempty
    <> Table.text "Country"     coviddataCountry
    <> Table.int "Cases"        coviddataCases
    <> Table.int "Deaths"       coviddataDeaths
    <> Table.int "Recoveries"   coviddataRecovered

countryTable :: Table App Country
countryTable = mempty
    <> Table.text "Abreviation" countryAlpha2
    <> Table.text "Country"     countryName    

covidTable2 :: Table App Coviddata2
covidTable2 = mempty
    <> Table.text "Country"     coviddata2Country
    <> Table.int "Cases"        coviddata2TotalConfirmed
    <> Table.int "Deaths"       coviddata2TotalDeaths
    <> Table.int "Recoveries"   coviddata2TotalRecovered

tableCasesDesc :: IO(HandlerFor App Html)
tableCasesDesc = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Desc CoviddataCases]
    return $ testHandler $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

countriesAsc :: IO([Country])
countriesAsc = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Asc CountryName]
    return $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

casesDesc :: IO([Coviddata])
casesDesc = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Desc CoviddataCases]
    return $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

casesDesc2 :: IO([Coviddata2])
casesDesc2 = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [Desc Coviddata2TotalConfirmed]
    return $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

--tableGeneralized :: Text -> SelectOpt Coviddata -> IO(HandlerFor App Html)
tableGeneralized orientation content = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    countries <- selectList [] [(orientation) (content)]
    return $ testHandler $ fmap (\Entity {entityKey=a, entityVal=b} -> b) countries

resetDB :: IO()
resetDB = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CoviddataCases >=. 0]
    deleteWhere [CountryNumeric >=. ""]
    return ()

resetDB2 :: IO()
resetDB2 = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [Coviddata2TotalConfirmed >=. 0]
    deleteWhere [CountryNumeric >=. ""]
    return ()

testHandler c = defaultLayout $ Table.buildBootstrap covidTable c

-- Here I want to get the data from the API into the Database
covidDataURL :: String
covidDataURL = "https://covid19-api.org/api/status"

covidData2URL :: String
covidData2URL= "https://api.covid19api.com/summary"

countryDataURL :: String
countryDataURL = "https://covid19-api.org/api/countries"

allCovidDataJSON :: IO B.ByteString
allCovidDataJSON = simpleHttp $ covidDataURL

allCovidData2JSON :: IO B.ByteString
allCovidData2JSON = simpleHttp $ covidData2URL

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

refreshDB2 :: IO()
refreshDB2 = do
    cov <- (eitherDecode <$> allCovidData2JSON) :: IO (Either String (Global, Object))

    case cov of
        Left err -> print err
        Right (_, d) ->  print d

refreshCoviddata :: [Coviddata] -> IO()
refreshCoviddata covject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
    deleteWhere [CoviddataCases >=. 0]
    ids <- forM covject insert
    return ()

refreshCoviddata2 :: [Coviddata2] -> IO()
refreshCoviddata2 covject = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll
--    deleteWhere [Coviddata2TotalConfirmed >=. 0]
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
    return ()
{--
testSelect' :: IO()
testSelect' = runSqlite "CovCheck.sqlite3" $ do
    runMigration migrateAll

    selection <- selectList [CountryName ==. "Afghanistan"] []
    print selection
--}