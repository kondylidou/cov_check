{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings     #-}

module Handler.Quiz where

import Import hiding (renderBootstrap)

import Control.Applicative ((<$>), (<*>))
import Data.Text           (Text)
import Data.Time           (Day, TimeOfDay (..))
import Yesod.Default.Util 
import Yesod.Form.Jquery

import Form.Bootstrap3


data QuizData = QuizData {
    name                :: Text, 
    surname             :: Text, 
    email               :: Text,
    age                 :: Int, 
    living              :: Bool,
    care                :: Bool,
    work                :: Bool,
    smoke               :: Bool,
    pregnancy           :: Bool,
    conf                :: Bool,
    fever               :: Bool,
    fever4              :: Bool,
    symptoms            :: Symptoms,
    breath              :: Bool,
    day                 :: Maybe Day,
    lung                :: Bool,
    diabetes            :: Bool,
    heart               :: Bool,
    obesity             :: Bool,
    steroids            :: Bool,
    immunosuppressants  :: Bool,
    flu                 :: Bool
    }
    deriving Show


-- The datatype we wish to receive from the form
fn :: Int -> Int
fn i | i > 0 = (-i)
fn i | otherwise = i


data Symptoms = Symptoms
    { chills        :: Maybe Bool
    , aches         :: Maybe Bool
    , loss          :: Maybe Bool
    , tired         :: Maybe Bool
    , cough         :: Maybe Bool
    , nose          :: Maybe Bool
    , diarrhea      :: Maybe Bool
    , throat        :: Maybe Bool
    , headache      :: Maybe Bool
    }
    deriving Show


hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Create user" }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm, submit = "Create user"}
bConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Create user" }
-- largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 4), submit = "Complete" }
largeFormConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Complete" }

bootstrapFieldHelper config label placeholder = bootstrapFieldSettings config label Nothing placeholder Nothing Nothing
{--
personHForm :: Html -> MForm Handler (FormResult Person, Widget)
personHForm = renderBootstrap hConfig $ Person
    <$> areq checkBoxField (bootstrapFieldHelper hConfig "Name" (Just "Person name")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper hConfig "Surname" (Just "Person surname")) Nothing

personIForm :: Html -> MForm Handler (FormResult Person, Widget)
personIForm = renderBootstrap iConfig $ Person
    <$> areq checkBoxField (bootstrapFieldHelper iConfig "Name" (Just "Person name")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper iConfig "Surname" (Just "Person surname")) Nothing


personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap bConfig $ Person
    <$> areq checkBoxField (bootstrapFieldHelper bConfig "Name" (Just "Person name")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper bConfig "Surname" (Just "Person surname")) Nothing
--}

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
    <*> areq boolField (bootstrapFieldHelper hConfig "In the past 24 hours, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "In the past 4 days, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
    <*> symptoms 
    <*> areq boolField (bootstrapFieldHelper hConfig "In the past 24 hours, did you feel that you were more quickly out of breath than usual?" (Just "Some bool")) Nothing
    <*> aopt (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:" -- 1900 till 0 years ago
        }) "With regard to all questions about symptoms: since when have you had the symptoms you specified?" Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with chronic lung disease by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with diabetes by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with heart disease by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with obesity by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking steroids?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking immunosuppressants?" (Just "bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been vaccinated against flu between October 2019 and today?" (Just "Some bool")) Nothing
    where
        symptoms = Symptoms 
            <$> aopt checkBoxField (bootstrapFieldHelper hConfig "Chills" (Just "Person chills")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Body aches" (Just "Person aches")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Loss of taste or smell" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Feeling tired or weak" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Persistent cough" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Runny nose" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Diarrhea" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Sore throat" (Just "Person loss")) Nothing
            <*> aopt checkBoxField (bootstrapFieldHelper hConfig "Headache" (Just "Person loss")) Nothing


-- The GET handler displays the form
getQuizR :: Handler Html
getQuizR = do
    (quizWidget, enctype) <- generateFormPost quizDataForm
    defaultLayout $ do
        --addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
        $(widgetFileReload def "Quiz")


postQuizR :: Handler Html
postQuizR = do
  ((result, quizWidget), enctype) <- runFormPost $ quizDataForm
  case result of
    FormSuccess formData -> do
      -- This is our success case branch
      setMessage "Form submitted successfully"
      return () 
    _ ->
      return ()
  defaultLayout $ do
    setTitle "Form processing sample"
    $(widgetFile "quiz")

    -- Generate the form to be displayed
    {--(widget, enctype) <- generateFormPost personForm 
    defaultLayout
        [whamlet|
            <p>
                The widget generated contains only the contents
                of the form, not the form tag itself. So...
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}  
            <form method=post action=@{PersonR} enctype=#{enctype}>
                ^{widget}   
                <p>It also doesn't include the submit button.
                <button>Submit
        |] 
--}
-- The POST handler processes the form. If it is successful, it displays the
-- parsed person. Otherwise, it displays the form again with error messages.
  {--postPersonR :: Handler Html
postPersonR = do
    ((result, widget), enctype) <- runFormPost personForm
    case result of
        FormSuccess person -> defaultLayout [whamlet|<p>#{show person}|]
        _ -> defaultLayout
            [whamlet|
                <p>Invalid input, let's try again.
                <form method=post action=@{PersonR} enctype=#{enctype}>
                    ^{widget}
                    <button>Submit
            |]
--}
