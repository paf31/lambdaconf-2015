-- | This module defines the views and application logic for the Programming Language
-- | Database application.
-- |
-- | The client is structured as a SPA (single-page application) written using the
-- | Thermite library.
-- |
-- | The page is defined by its _state_, and we define _actions_ which act on those
-- | states. Actions can be connected to buttons or other inputs, as we will see.

module UI where

import Debug.Trace

import Data.Int
import Data.Array (concatMap, map)
import Data.Maybe
import Data.Either
import Data.String (split, joinWith)

import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as H
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

import UI.Types
import UI.AJAX
import UI.Utils

-- | The type of application states.
-- |
-- | Our state type is a sum, with one constructor for each subpage.
-- | 
-- | There are four main subpages:
-- |
-- | - The main page, which shows a list of languages and tags.
-- | - A page for viewing a single language in detail
-- | - A page for viewing languages with a selected tag
-- | - A page for creating and editing languages
-- |
-- | We also define subpages for errors and the loading message.
data State 
  = Home [LangSummary] [TagSummary]
  | ViewLang Lang
  | ViewTag Tag [LangSummary]
  | EditLang Lang
  | Loading
  | Error String

data Action
  = LoadList
  | LoadLang Key
  | LoadTag Tag
  | LoadNewLang
  | LoadEditLang Lang
  | UpdateForm (Lang -> Lang)
  | SaveLang
  
initialState :: State
initialState = Loading

render :: T.Render _ State _ Action
render ctx st _ _ =
  H.div (A.className "container") (header : renderPage st)
  where
  header :: T.Html _
  header = H.div (A.className "navbar navbar-default")
             [ H.div (A.className "navbar-header") 
               [ H.a (A.href "#" <> A.className "navbar-brand" <> T.onClick ctx (const LoadList)) 
                 [ T.text "Programming Languages Database" ] ]
             ]
      
  renderPage :: State -> [T.Html _]
  renderPage Loading = [ T.text "Loading..." ]
  renderPage (Error err) = [ T.text err ]
  renderPage (Home langs tags) = 
    [ H.h2' [ T.text "Tags" ]
    , renderTags (map (_.tag <<< runTagSummary) tags)
    , H.h2' [ T.text "Languages" ]
    , renderSummaries langs 
    , editLangBtn "Add Language" emptyLang
    ]
  renderPage (ViewLang lang@(Lang l)) = 
    [ H.h2' [ T.text l.name ]
    , renderTags l.tags
    , H.p (A.className "lead") [ T.text l.description ]
    , H.p' [ H.a (A.href l.homepage) [ T.text l.homepage ] ]
    , ratingsButton lang
    , editLangBtn "Add Language" lang
    ]
  renderPage (ViewTag tag langs) = 
    [ H.h2' [ T.text ("Languages Tagged " <> show tag) ]
    , renderSummaries langs
    ]
  renderPage (EditLang newLang) =
    [ H.h2' [ T.text "Edit Language" ]
    , editLangForm newLang
    ]

  renderSummaries :: [LangSummary] -> T.Html _
  renderSummaries = H.ul' <<< map (renderSummary <<< runLangSummary)
    where
    renderSummary summary = H.li' [ H.a (A.href "#" <> T.onClick ctx (const (LoadLang summary.key))) 
                                    [ T.text summary.name ] 
                                  ]

  renderTags :: [Tag] -> T.Html _
  renderTags = H.p' <<< concatMap renderTag
    where
    renderTag tag = [ H.a (A.href "#" <> A.className "label label-default" <> T.onClick ctx (const (LoadTag tag))) 
                        [ T.text tag ] 
                    , T.text " "
                    ]                   
           
  editLangBtn :: String -> Lang -> T.Html _
  editLangBtn text lang = 
    H.p' [ H.small' [ H.a (A.href "#" 
                           <> T.onClick ctx (const (LoadEditLang lang))) 
                          [ T.text text ] 
                    ]
         ]
                    
  ratingsButton :: Lang -> T.Html _
  ratingsButton lang = 
    let likes = toNumber ((runLang lang).rating) in 
    H.p' [ H.button (A.className "btn btn-primary")
                    [ H.span (A.className "badge") [ T.text (show likes) ]
                    , T.text " Likes"
                    ] 
         ]
                    
  editLangForm :: Lang -> T.Html _
  editLangForm (Lang lang) = 
    H.form (A.className "form-horizontal")
      [ H.div (A.className "form-group")
        [ H.label (A.className "col-sm-2 control-label")
          [ T.text "ID" ]
        , H.div (A.className "col-sm-4")
          [ H.input (A._type "text" 
                     <> A.className "form-control"
                     <> A.placeholder "A unique identifier for this language" 
                     <> A.value lang.key
                     <> T.onInput ctx (\e -> UpdateForm (Lang <<< _ { key = inputValue e } <<< runLang))) [] ]
        ]
      , H.div (A.className "form-group")
        [ H.label (A.className "col-sm-2 control-label")
          [ T.text "Name" ]
        , H.div (A.className "col-sm-4")
          [ H.input (A._type "text" 
                     <> A.className "form-control" 
                     <> A.placeholder "The name of this programming language" 
                     <> A.value lang.name
                     <> T.onInput ctx (\e -> UpdateForm (Lang <<< _ { name = inputValue e } <<< runLang))) [] ]
        ]
      , H.div (A.className "form-group")
        [ H.label (A.className "col-sm-2 control-label")
          [ T.text "Description" ]
        , H.div (A.className "col-sm-4")
          [ H.input (A._type "text"
                     <> A.className "form-control"
                     <> A.placeholder "A short description"
                     <> A.value lang.description
                     <> T.onInput ctx (\e -> UpdateForm (Lang <<< _ { description = inputValue e } <<< runLang))) [] ]
        ]
      , H.div (A.className "form-group")
        [ H.label (A.className "col-sm-2 control-label")
          [ T.text "Homepage" ]
        , H.div (A.className "col-sm-4")
          [ H.input (A._type "text"
                     <> A.className "form-control"
                     <> A.placeholder "The main website for this language"
                     <> A.value lang.homepage
                     <> T.onInput ctx (\e -> UpdateForm (Lang <<< _ { homepage = inputValue e } <<< runLang))) [] ]
        ]
      , H.div (A.className "form-group")
        [ H.label (A.className "col-sm-2 control-label")
          [ T.text "Tags" ]
        , H.div (A.className "col-sm-4")
          [ H.input (A._type "text"
                     <> A.className "form-control"
                     <> A.placeholder "A list of tags separated by spaces"
                     <> A.value (joinWith " " lang.tags)
                     <> T.onInput ctx (\e -> UpdateForm (Lang <<< _ { tags = split " " (inputValue e) } <<< runLang))) [] ]
        ]
      , H.div (A.className "form-group")
        [ H.div (A.className "col-sm-offset-2 col-sm-4")
          [ H.button (A.className "btn btn-primary"
                      <> T.onClick ctx (const SaveLang)) 
                     [ T.text "Save" ] ]
        ]
      ]

performAction :: T.PerformAction _ State _ Action
performAction _ LoadList = do
  langs <- listLangs
  tags <- listTags
  let listData = Home <$> langs <*> tags
  T.setState (either Error id listData)
performAction _ (LoadLang key) = do
  T.setState $ Loading
  lang <- getLang key
  T.setState (either Error ViewLang lang)
performAction _ (LoadTag tag) = do
  T.setState $ Loading
  langs <- getTag tag
  T.setState (either Error (ViewTag tag) langs)
performAction _ (LoadEditLang lang) =
  T.setState (EditLang lang)
performAction _ (UpdateForm f) = do
  EditLang lang <- T.getState
  T.setState (EditLang (f lang))
performAction props SaveLang = do
  EditLang lang <- T.getState
  response <- putLang lang
  case response of
    Left err -> T.setState (Error err)
    Right Ok -> performAction props LoadList

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount LoadList

main = do
  let component = T.createClass spec
  T.render component unit