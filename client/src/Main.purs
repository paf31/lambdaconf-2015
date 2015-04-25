module Main where

import Debug.Trace

import Data.Int
import Data.Array (concatMap, map)
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Eff
import Control.Monad.Aff

import Network.HTTP.Affjax

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
  { key :: String
  , name :: String
  }
  
runLangSummary (LangSummary o) = o

instance langSummaryIsForeign :: IsForeign LangSummary where
  read value = LangSummary <$> 
    ({ key: _, name: _ }
      <$> readProp "key" value
      <*> readProp "name" value)

newtype Lang = Lang
  { name :: Key
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [Tag]
  }

instance langIsForeign :: IsForeign Lang where
  read value = Lang <$> 
    ({ name: _, description: _, homepage: _, rating: _, tags: _ }
      <$> readProp "name" value
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
  | Loading
  | Error String

data Action
  = LoadList
  | LoadLang Key
  | LoadTag Tag

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
    , H.h2' [ T.text "Tags" ]
    , renderTags (map (_.tag <<< runTagSummary) tags)
    ]
  renderPage (ViewLang (Lang lang)) = 
    [ H.h2' [ T.text lang.name ]
    , H.p (A.className "lead") [ T.text lang.description ]
    , renderTags lang.tags
    ]
  renderPage (ViewTag tag langs) = 
    [ H.h2' [ T.text "Tag "
            , H.span (A.className "label label-default") [ T.text tag ] 
            ]
    , renderSummaries langs
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

ajax :: forall eff state a. (IsForeign a) => Affjax eff Foreign -> T.Action (ajax :: AJAX | eff) state (Either String a)
ajax aff = T.async \k -> runAff (\_ -> k (Left "Web service call failed"))
                                (\r -> k (readWith (const "Cannot parse response") r.response))
                                aff

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render
         # T.componentWillMount LoadList

main = do
  let component = T.createClass spec
  T.render component unit