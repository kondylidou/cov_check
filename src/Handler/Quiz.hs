{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Handler.Quiz where

import Import hiding (renderBootstrap)

import Control.Applicative ((<$>), (<*>))
import Data.Text           (Text)
import Data.Time           (Day, TimeOfDay (..))
import Yesod.Default.Util 
import Yesod.Form.Jquery
import GHC.Generics
import qualified Data.Text as T

import Form.Bootstrap3

-- Define our data that will be used for creating the form
data QuizData = QuizData 
    { name                :: Text
    , surname             :: Text
    , email               :: Text
    , age                 :: Int
    , living              :: Bool
    , care                :: Bool
    , work                :: Bool
    , smoke               :: Bool
    , pregnancy           :: Bool
    , conf                :: Bool
    , symptoms            :: Symptoms
    , record              :: Record
    }
   deriving (Show,Generic)


data Symptoms = Symptoms
    { fever         :: Bool
    , fever4        :: Bool
    , chills        :: Bool
    , aches         :: Bool
    , loss          :: Bool
    , tired         :: Bool
    , cough         :: Bool
    , nose          :: Bool
    , diarrhea      :: Bool
    , throat        :: Bool
    , headache      :: Bool
    , breath        :: Bool
    }
    deriving Show


data Record = Record
    { lung                :: Bool
    , diabetes            :: Bool
    , heart               :: Bool
    , obesity             :: Bool
    , steroids            :: Bool
    , immunosuppressants  :: Bool
    , flu                 :: Bool
    }
    deriving Show


hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Create user" }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm, submit = "Create user"}
bConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Create user" }
-- largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 4), submit = "Complete" }
largeFormConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Complete" }

bootstrapFieldHelper config label placeholder = bootstrapFieldSettings config label Nothing placeholder Nothing Nothing

quizDataForm :: Html -> MForm Handler (FormResult QuizData, Widget)
quizDataForm = renderBootstrap largeFormConfig $ QuizData
    <$> areq textField (bootstrapFieldHelper iConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper iConfig "Surname" (Just "Person surname")) Nothing
    <*> areq textField (bootstrapFieldHelper iConfig "Email" (Just "Person email")) Nothing
    <*> areq intField  (bootstrapFieldHelper iConfig "Age" (Just "0")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you living alone?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "At least once a week, do you privately care for people with age-related conditions, chronic illnesses, or frailty?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Do you work in the medical field or in a community facility?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Do you smoke?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you pregnant?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you had close contact with a confirmed case?" (Just "Some bool")) Nothing
    <*> symptoms
    <*> record
    where
        symptoms = Symptoms
            <$> areq boolField (bootstrapFieldHelper hConfig "In the past 24 hours, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "In the past 4 days, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Chills" (Just "Person chills")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Body aches" (Just "Person aches")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Loss of taste or smell" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Feeling tired or weak" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Persistent cough" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Runny nose" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Diarrhea" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Sore throat" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Headache" (Just "Person loss")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "In the past 24 hours, did you feel that you were more quickly out of breath than usual?" (Just "Some bool")) Nothing
        record = Record
            <$> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with chronic lung disease by a doctor?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with diabetes by a doctor?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with heart disease by a doctor?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with obesity by a doctor?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking steroids?" (Just "Some bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking immunosuppressants?" (Just "bool")) Nothing
            <*> areq boolField (bootstrapFieldHelper hConfig "Have you been vaccinated against flu between October 2019 and today?" (Just "Some bool")) Nothing


-- The GET handler displays the form
getQuizR :: Handler Html
getQuizR = do
    (quizWidget, enctype) <- generateFormPost quizDataForm
    let submission = Nothing :: Maybe QuizData
        handlerName = "getQuizR" :: Text
    defaultLayout $ do
        --addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
        setTitle "Quiz"
        $(widgetFileReload def "Quiz")


{-- 
-- The POST handler processes the form
postQuizR :: Handler Html
postQuizR = do
    ((result, quizWidget), enctype) <- runFormPost quizDataForm
    let handlerName = "postQuizR" :: Text
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
        $(widgetFile "quiz")

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




--}