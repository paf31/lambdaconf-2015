module Main where

import Debug.Trace

import Data.Int
import Data.Array (concatMap, map)
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class
import Data.String (split, joinWith)

import Control.Monad.Eff
import Control.Monad.Eff.Exception (Error(), error)
import Control.Monad.Aff
import Control.Monad.Error.Class (throwError)

import Network.HTTP.Affjax
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(..))

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as H
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

type Key = String

type Tag = String

newtype LangSummary = LangSummary
  { key :: Key
  , name :: String
  }
  
runLangSummary (LangSummary o) = o

instance langSummaryIsForeign :: IsForeign LangSummary where
  read value = LangSummary <$> 
    ({ key: _, name: _ }
      <$> readProp "key" value
      <*> readProp "name" value)

newtype Lang = Lang
  { key :: Key
  , name :: String
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [Tag]
  }
  
emptyLang :: Lang
emptyLang = Lang
  { key: ""
  , name: ""
  , description: ""
  , homepage: ""
  , rating: zero :: Int
  , tags: []
  }

instance langIsForeign :: IsForeign Lang where
  read value = Lang <$> 
    ({ key: _, name: _, description: _, homepage: _, rating: _, tags: _ }
      <$> readProp "key" value
      <*> readProp "name" value
      <*> readProp "description" value
      <*> readProp "homepage" value
      <*> (fromNumber <$> readProp "rating" value)
      <*> readProp "tags" value)

newtype TagSummary = TagSummary
  { tag :: Tag
  }
  
runTagSummary (TagSummary o) = o

instance tagSummaryIsForeign :: IsForeign TagSummary where
  read value = TagSummary <<< { tag: _ } <$> readProp "tag" value
      
runLang (Lang o) = o

data State 
  = ListLangs [LangSummary] [TagSummary]
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
  renderPage (ListLangs langs tags) = 
    [ H.h2' [ T.text "Languages" ]
    , renderSummaries langs 
    , H.p' [ H.small' [ H.a (A.href "#" <> T.onClick ctx (const (LoadEditLang emptyLang))) [ T.text "Add Language" ] ] ]
    , H.h2' [ T.text "Tags" ]
    , renderTags (map (_.tag <<< runTagSummary) tags)
    ]
  renderPage (ViewLang (Lang lang)) = 
    [ H.h2' [ T.text lang.name ]
    , H.p (A.className "lead") [ T.text lang.description ]
    , H.p' [ H.a (A.href lang.homepage) [ T.text lang.homepage ] ]
    , H.p' [ H.small' [ H.a (A.href "#" <> T.onClick ctx (const (LoadEditLang (Lang lang)))) [ T.text "Edit" ] ] ]
    , renderTags lang.tags
    ]
  renderPage (ViewTag tag langs) = 
    [ H.h2' [ T.text "Tag "
            , H.span (A.className "label label-default") [ T.text tag ] 
            ]
    , renderSummaries langs
    ]
  renderPage (EditLang newLang) =
    [ H.h2' [ T.text "Add Language" ]
    , addLang newLang
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
                    
  addLang :: Lang -> T.Html _
  addLang (Lang lang) = 
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
  langs <- ajax $ get "/api/lang/"
  tags <- ajax $ get "/api/tag/"
  let listData = ListLangs <$> langs <*> tags
  T.setState (either Error id listData)
performAction _ (LoadLang key) = do
  T.setState $ Loading
  lang <- ajax <<< get $ "/api/lang/" <> key
  T.setState (either Error ViewLang lang)
performAction _ (LoadTag tag) = do
  T.setState $ Loading
  langs <- ajax <<< get $ "/api/tag/" <> tag
  T.setState (either Error (ViewTag tag) langs)
performAction _ (LoadEditLang lang) =
  T.setState (EditLang lang)
performAction _ (UpdateForm f) = do
  EditLang lang <- T.getState
  T.setState (EditLang (f lang))
performAction props SaveLang = do
  EditLang lang <- T.getState
  response <- ajax $ affjax $ 
    defaultRequest { method = PUT
                   , url = "/api/lang/" <> (runLang lang).key
                   , content = Just (stringify lang)
                   , headers = [ContentType (MimeType "application/json")]
                   }
  case response of
    Left err -> T.setState (Error err)
    Right Ok -> performAction props LoadList

data Ok = Ok

instance isForeignOk :: IsForeign Ok where
  read _ = return Ok

foreign import stringify
  "function stringify(lang) {\
  \  return JSON.stringify(lang);\
  \}" :: Lang -> String

foreign import inputValue
  "function inputValue(e) {\
  \  return e.target.value;\
  \}" :: T.FormEvent -> String

ajax :: forall eff state a. (IsForeign a) => Affjax eff Foreign -> T.Action (ajax :: AJAX | eff) state (Either String a)
ajax aff = T.async \k -> runAff (\_ -> k (Left "Web service call failed"))
                                (\r -> k (readWith (const "Cannot parse response") r))
                                aff1
  where
  aff1 = do
    r <- aff
    case r.status of
      StatusCode n | n >= fromNumber 200 && n < fromNumber 300 -> return r.response
      _ -> throwError (error "Bad status code")

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount LoadList

main = do
  let component = T.createClass spec
  T.render component unit