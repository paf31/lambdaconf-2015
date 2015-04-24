module Main where

import Debug.Trace

import Control.Monad.Eff

import qualified Thermite as T
import qualified Thermite.Html as T
import qualified Thermite.Html.Elements as H
import qualified Thermite.Html.Attributes as A
import qualified Thermite.Events as T
import qualified Thermite.Action as T
import qualified Thermite.Types as T

data State = State

data Action

initialState :: State
initialState = State

render :: T.Render _ State _ Action
render ctx State _ _ =
  H.div (A.className "container") 
    [ H.h1' [ T.text "Exercises" ]
    , H.ol' 
        [ H.li' [ T.text "Modify the state to include an integer counter." ]
        , H.li' [ T.text "Add a label below to display the current state." ]
        , H.li' [ T.text "Add a button to increment the counter" ]
        , H.li' [ T.text "Add a button to reset the counter" ]
        ]
    ]

performAction :: T.PerformAction _ State _ Action
performAction s _ = pure s

spec :: T.Spec _ State _ Action
spec = T.simpleSpec initialState performAction render

main = do
  let component = T.createClass spec
  T.render component unit
