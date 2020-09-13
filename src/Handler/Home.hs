{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "CovCheck Homepage"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To CovCheck!"
        $(widgetFile "homepage")

