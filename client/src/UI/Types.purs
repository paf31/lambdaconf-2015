module UI.Types where

import Data.Int
import Data.Foreign
import Data.Foreign.Class
    
type Key = String

type Tag = String
  
data Ok = Ok

newtype TagSummary = TagSummary
  { tag :: Tag
  }
  
runTagSummary (TagSummary o) = o

newtype LangSummary = LangSummary
  { key :: Key
  , name :: String
  }
  
runLangSummary (LangSummary o) = o

newtype Lang = Lang
  { key :: Key
  , name :: String
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [Tag]
  }
      
runLang (Lang o) = o
  
emptyLang :: Lang
emptyLang = Lang
  { key: ""
  , name: ""
  , description: ""
  , homepage: ""
  , rating: zero :: Int
  , tags: []
  }

instance isForeignOk :: IsForeign Ok where
  read _ = return Ok

instance tagSummaryIsForeign :: IsForeign TagSummary where
  read value = TagSummary <<< { tag: _ } <$> readProp "tag" value

instance langSummaryIsForeign :: IsForeign LangSummary where
  read value = LangSummary <$> 
    ({ key: _, name: _ }
      <$> readProp "key" value
      <*> readProp "name" value)

instance langIsForeign :: IsForeign Lang where
  read value = Lang <$> 
    ({ key: _, name: _, description: _, homepage: _, rating: _, tags: _ }
      <$> readProp "key" value
      <*> readProp "name" value
      <*> readProp "description" value
      <*> readProp "homepage" value
      <*> (fromNumber <$> readProp "rating" value)
      <*> readProp "tags" value)