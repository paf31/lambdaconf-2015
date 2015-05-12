-- | Some helper functions

module UI.Utils where
    
import UI.Types

import qualified Thermite.Events as T
    
-- | Convert a language record to a JSON document.
foreign import stringify
  "function stringify(lang) {\
  \  return JSON.stringify(lang);\
  \}" :: Lang -> String

-- | Get the current value from an input field.
foreign import inputValue
  "function inputValue(e) {\
  \  return e.target.value;\
  \}" :: T.FormEvent -> String