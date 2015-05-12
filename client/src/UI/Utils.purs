module UI.Utils where
    
import UI.Types

import qualified Thermite.Events as T
    
foreign import stringify
  "function stringify(lang) {\
  \  return JSON.stringify(lang);\
  \}" :: Lang -> String

foreign import inputValue
  "function inputValue(e) {\
  \  return e.target.value;\
  \}" :: T.FormEvent -> String