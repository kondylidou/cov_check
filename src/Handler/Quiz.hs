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

data LargeData = LargeData {
    textField1       :: Text,
    age              :: Int,
    living           :: Bool,
    care             :: Bool,
    work             :: Bool,
    smoke            :: Bool,
    pregnancy        :: Bool,
    conf             :: Bool,
    fever            :: Bool,
    fever4           :: Bool,
    checkboxField1   :: Bool
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


hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Create user" }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm, submit = "Create user"}
bConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Create user" }
largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 4), submit = "Submit" }

bootstrapFieldHelper config label placeholder = bootstrapFieldSettings config label Nothing placeholder Nothing Nothing

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap hConfig $ Person
    <$> areq textField (bootstrapFieldHelper hConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Surname" (Just "Person surname")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "Age" (Just "0")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Email" (Just "Person email")) Nothing


largeDataForm :: Html -> MForm Handler (FormResult LargeData, Widget)
largeDataForm = renderBootstrap largeFormConfig $ LargeData
    <$> areq textField (bootstrapFieldHelper hConfig "Text" (Just "Some text content")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "How old are you?" (Just "0")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Are you living alone?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "At least once a week, do you privately care for people with age-related conditions, chronic illnesses, or frailty?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Do you work in the medical field or in a community facility?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Do you smoke?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Is there a pregnancy possibility?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Have you had close contact with a confirmed case?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "In the past 24 hours, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "In the past 4 days, have you had a fever (over 38°C)?" (Just "Some bool")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper hConfig "Checkbox" (Just "Some checkbox")) Nothing
    {-- <*> areq [textField] (bootstrapFieldHelper hConfig "Colors" (Just "Some checkbox")) Nothing --}

questionForm :: Html -> MForm Handler (FormResult Questions, Widget)
questionForm = renderBootstrap largeFormConfig $ Questions
    <$> areq boolField (bootstrapFieldHelper iConfig 
    "At least once a week, do you privately care for people with age-related conditions, chronic illnesses, or frailty?" (Just "Some text content")) Nothing



-- The GET handler displays the form
getQuizR :: Handler Html
getQuizR = do
    (basicWidget, enctype) <- generateFormPost personForm
    (largeWidget, enctype) <- generateFormPost largeDataForm
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

postPersonR :: Handler Html
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

