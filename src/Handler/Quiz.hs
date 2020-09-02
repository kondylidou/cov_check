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
    intField1        :: Int,
    doubleField1     :: Double,
    textAreaField1   :: Textarea,
    hiddenField1     :: Text,
    passwordField1   :: Text,
    emailField1      :: Text,
    htmlField1       :: Html,
    dayField1        :: Day,
    timeField1       :: TimeOfDay,
    searchField1     :: Text,
    urlField1        :: Text,
    selectField1     :: Bool,
    checkboxField1   :: Bool
    }


-- The datatype we wish to receive from the form
data Person = Person
    { personName    :: Text
    , personSurname :: Text
    }
    {--, personAge     :: Int
    , personColor   :: Color
    , personContent :: Textarea
    --}
  deriving Show


data Questions = Questions
    { typ           :: Text
    , name          :: Text
    , title         :: Text
    , colCount      :: Int
    , isRequired    :: Bool
    }
    deriving Show



data Color = Red | Blue | Gray | Black
    deriving (Show, Eq, Enum, Bounded)


hConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Create user" }
iConfig = BootstrapFormConfig { form = BootstrapInlineForm, submit = "Create user"}
bConfig = BootstrapFormConfig { form = BootstrapBasicForm, submit = "Create user" }
largeFormConfig = BootstrapFormConfig { form = BootstrapHorizontalForm (ColXs 2) (ColXs 4) (ColXs 2), submit = "Submit large data" }

bootstrapFieldHelper config label placeholder = bootstrapFieldSettings config label Nothing placeholder Nothing Nothing

personHForm :: Html -> MForm Handler (FormResult Person, Widget)
personHForm = renderBootstrap hConfig $ Person
    <$> areq textField (bootstrapFieldHelper hConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper hConfig "Surname" (Just "Person surname")) Nothing

personIForm :: Html -> MForm Handler (FormResult Person, Widget)
personIForm = renderBootstrap iConfig $ Person
    <$> areq textField (bootstrapFieldHelper iConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper iConfig "Surname" (Just "Person surname")) Nothing

personForm :: Html -> MForm Handler (FormResult Person, Widget)
personForm = renderBootstrap bConfig $ Person
    <$> areq textField (bootstrapFieldHelper bConfig "Name" (Just "Person name")) Nothing
    <*> areq textField (bootstrapFieldHelper bConfig "Surname" (Just "Person surname")) Nothing

largeDataForm :: Html -> MForm Handler (FormResult LargeData, Widget)
largeDataForm = renderBootstrap largeFormConfig $ LargeData
    <$> areq textField (bootstrapFieldHelper hConfig "Text" (Just "Some text content")) Nothing
    <*> areq intField (bootstrapFieldHelper hConfig "Int" (Just "Some integer value")) Nothing
    <*> areq doubleField (bootstrapFieldHelper hConfig "Double" (Just "Some double value")) Nothing
    <*> areq textareaField (bootstrapFieldHelper hConfig "Area" (Just "Some text area content")) Nothing
    <*> areq hiddenField (bootstrapFieldHelper hConfig "Hidden" (Just "Hidden field")) Nothing
    <*> areq passwordField (bootstrapFieldHelper hConfig "Password" (Just "Password field")) Nothing
    <*> areq emailField (bootstrapFieldHelper hConfig "Email" (Just "Email field")) Nothing
    <*> areq htmlField (bootstrapFieldHelper hConfig "Html" (Just "Some HTML")) Nothing
    <*> areq dayField (bootstrapFieldHelper hConfig "Day" (Just "Some day")) Nothing
    <*> areq timeField (bootstrapFieldHelper hConfig "Time" (Just "Some time")) Nothing
    <*> areq (searchField False) (bootstrapFieldHelper hConfig "Search" (Just "Some search")) Nothing
    <*> areq urlField (bootstrapFieldHelper hConfig "URL" (Just "Some URL")) Nothing
    <*> areq boolField (bootstrapFieldHelper hConfig "Bool" (Just "Some bool")) Nothing
    <*> areq checkBoxField (bootstrapFieldHelper hConfig "Checkbox" (Just "Some checkbox")) Nothing




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
    (inlineWidget, enctype) <- generateFormPost personIForm
    (horizontalWidget, enctype) <- generateFormPost personHForm
    (largeWidget, enctype) <- generateFormPost largeDataForm
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

