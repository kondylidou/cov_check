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
import Data.Vector() 
import Yesod.Default.Util
import Yesod.Form.Jquery

import Form.Bootstrap3

data QuizData = QuizData {
    name                :: Text,
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
    day                 :: Day,
    lung                :: Bool,
    diabetes            :: Bool,
    heart               :: Bool,
    obesity             :: Bool,
    steroids            :: Bool,
    immunosuppressants  :: Bool,
    flu                 :: Bool
    }


-- The datatype we wish to receive from the form
data Person = Person
    { personName    :: Text
    , personSurname :: Text
    , personAge     :: Int
    , personEmail   :: Text
    }
    deriving Show

data Questions = Questions
    { question      :: Bool
    }
    deriving Show


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
largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 4), submit = "Complete" }

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

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap hConfig $ Person
    <$> areq textField (bootstrapFieldHelper hConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Surname" (Just "Person surname")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "Age" (Just "0")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Email" (Just "Person email")) Nothing

questionForm :: Html -> MForm Handler (FormResult Questions, Widget)
questionForm = renderBootstrap largeFormConfig $ Questions
    <$> areq boolField (bootstrapFieldHelper iConfig 
    "At least once a week, do you privately care for people with age-related conditions, chronic illnesses, or frailty?" (Just "Some text content")) Nothing

quizDataForm :: Html -> MForm Handler (FormResult QuizData, Widget)
quizDataForm = renderBootstrap largeFormConfig $ QuizData
    <$> areq textField (bootstrapFieldHelper hConfig "What is your name?" (Just "Some name")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "How old are you?" (Just "0")) Nothing
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
    <*> areq (jqueryDayField def
        { jdsChangeYear = True -- give a year dropdown
        , jdsYearRange = "1900:-5" -- 1900 till five years ago
        }) "With regard to all questions about symptoms: since when have you had the symptoms you specified?" Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with chronic lung disease by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with diabetes by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with heart disease by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you been diagnosed with obesity by a doctor?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking steroids?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you currently taking immunosuppressants?" (Just "Some bool")) Nothing
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
-- Declare the form. The type signature is a bit intimidating, but here's the
-- overview:
--
-- * The Html parameter is used for encoding some extra information. See the
-- discussion regarding runFormGet and runFormPost below for further
-- explanation.
--
-- * We have our Handler as the inner monad, which indicates which site this is
-- running in.
--
-- * FormResult can be in three states: FormMissing (no data available),
-- FormFailure (invalid data) and FormSuccess
--
-- * The Widget is the viewable form to place into the web page.
--
-- Note that the scaffolded site provides a convenient Form type synonym,
-- so that our signature could be written as:
--
-- > personForm :: Form Person
--
-- For our purposes, it's good to see the long version.
{-- personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderDivs $ Person 
    <$> areq textField "What's your name?" Nothing
    <*> areq intField "How old are you?" Nothing
    <*> areq (radioField optionsEnum) "Color" Nothing
    <*> areq textareaField "What is your current living situation?" Nothing
--}

-- The GET handler displays the form
getQuizR :: Handler Html
getQuizR = do
    (basicWidget, enctype) <- generateFormPost personForm
    --(inlineWidget, enctype) <- generateFormPost personIForm
    --(horizontalWidget, enctype) <- generateFormPost personHForm
    (quizWidget, enctype) <- generateFormPost quizDataForm
    (questionWidget, enctype) <- generateFormPost questionForm
    defaultLayout $ do
        addStylesheetRemote "//netdna.bootstrapcdn.com/bootstrap/3.1.0/css/bootstrap.min.css"
        $(widgetFileReload def "Quiz")

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
