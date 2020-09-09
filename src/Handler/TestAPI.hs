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

getTestAPIR :: Handler Html
getTestAPIR = do
    defaultLayout $ do
        setTitle "API Testpage"
        $(widgetFile "testAPI")
{--
-- Here I want to get the data from the API into the Database
allCountriesURL :: String
allCountriesURL = "https://api.thevirustracker.com/free-api?countryTotals=ALL"

allCountriesJSON :: IO (Maybe Countrydata)
allCountriesJSON = fmap decode $ simpleHttp $ allCountriesURL
--}