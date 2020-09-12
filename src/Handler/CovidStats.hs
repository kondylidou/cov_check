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
import Model (Coviddata)


getCovidStatsR :: Handler Html
getCovidStatsR = do
        defaultLayout $ do
            setTitle "Global Covid-19 Statistics"
            $(widgetFile "covidStats")