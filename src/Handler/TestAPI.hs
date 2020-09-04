{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.TestAPI where

import Import
import Text.Julius()

getTestAPIR :: Handler Html
getTestAPIR = do
    defaultLayout $ do
        setTitle "API Testpage"
        $(widgetFile "testAPI")