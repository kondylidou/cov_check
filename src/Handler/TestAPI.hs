{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.TestAPI where

import Import

getTestAPIR :: Handler Html
getTestAPIR = do
    defaultLayout $ do
        setTitle "API Testpage"
        $(widgetFile "testAPI")