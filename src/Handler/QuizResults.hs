{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Handler.QuizResults where

import Import hiding (renderBootstrap)

import Control.Applicative ((<$>), (<*>))
import Data.Text           (Text)
import Data.Time           (Day, TimeOfDay (..))
import Yesod.Default.Util 
import Yesod.Form.Jquery
import GHC.Generics
import qualified Data.Text as T

import Handler.Quiz
import Form.Bootstrap3


-- The POST handler processes the form
postQuizResultsR :: Handler Html
postQuizResultsR = do
    ((result, quizWidget), enctype) <- runFormPost quizDataForm
    let handlerName = "postQuizResultsR" :: Text
        submission = case result of
            FormSuccess res -> case (conf res) of 
                True -> Just res
                _ -> case processSymptoms (symptoms res) of
                    True -> Just res
                    _ -> case processRecord (record res) of
                        True -> Just res
                        _ ->  Nothing
            _ -> Nothing

    defaultLayout $ do
        setTitle "Quiz Result"
        $(widgetFile "quizResults")

processSymptoms :: Symptoms -> Bool
processSymptoms symptoms 
    | fever symptoms == True = True
    | fever4 symptoms == True = True
    | cough symptoms && tired symptoms == True = True
    | cough symptoms && diarrhea symptoms == True = True
    | cough symptoms && headache symptoms == True = True
    | cough symptoms && loss symptoms == True = True
    | tired symptoms && diarrhea symptoms == True = True
    | tired symptoms && headache symptoms == True = True
    | tired symptoms && loss symptoms == True = True
    | diarrhea symptoms && loss symptoms == True = True
    | diarrhea symptoms && headache symptoms == True = True
    | headache symptoms && loss symptoms == True = True
    | otherwise = False

processRecord :: Record -> Bool
processRecord record 
    | lung record == True = True
    | diabetes record == True = True
    | heart record == True = True
    | obesity record == True = True
    | steroids record == True = True
    | immunosuppressants record == True = True
    | flu record == True = True
    | otherwise = False




