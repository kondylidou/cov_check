{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE DeriveGeneric #-}
module Handler.HtmlStats where

import Import
import Text.Julius()

getHtmlStatsR :: Handler Html
getHtmlStatsR = do
    defaultLayout $ do
        setTitle "Global Covid-19 Statistics"
        $(widgetFile "htmlStats")
