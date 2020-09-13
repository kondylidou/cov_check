{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Handler.QuizResultSpec (spec) where

import TestImport

spec :: Spec
spec = withApp $ do

          request $ do
              setMethod "POST"
              setUrl CovidStatsR
              

          statusIs 200
          -- more debugging printBody
          htmlAllContain ".upload-response" "text/plain"
          htmlAllContain ".upload-response" "Some Content"
