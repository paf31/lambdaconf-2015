module DB 
  ( Insensitive()
  , insensitive
  , runInsensitive
  , Key()
  , Tag()
  , Lang(..)
  , runLang
  , DB()
  , db
  ) where

import Data.Int
import Data.Tuple
import Data.Array (map)
import Data.Function (on)
import Data.String (toLower)

import qualified Data.Map as M

newtype Insensitive = Insensitive String

insensitive s = Insensitive (toLower s)

runInsensitive (Insensitive s) = s

instance eqInsensitive :: Eq Insensitive where
  (==) = (==) `on` runInsensitive
  (/=) = (/=) `on` runInsensitive

instance ordInsensitive :: Ord Insensitive where
  compare = compare `on` runInsensitive
  
type Key = Insensitive

type Tag = Insensitive

newtype Lang = Lang
  { key :: Key
  , name :: String
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [Tag]
  }
  
runLang (Lang lang) = lang
    
type DB = M.Map Key Lang

-- Ratings based on number of GitHub search results

purescript :: Lang
purescript = Lang
  { key: Insensitive "purescript"
  , name: "PureScript"
  , description: "A small strongly typed programming language that compiles to JavaScript"
  , homepage: "http://purescript.org/"
  , rating: fromNumber 463
  , tags: map insensitive [ "Pure", "Functional", "Static", "AltJS" ]
  }
  
haskell :: Lang
haskell = Lang
  { key: Insensitive "haskell"
  , name: "Haskell"
  , description: "An advanced purely-functional programming language"
  , homepage: "http://haskell.org/"
  , rating: fromNumber 16586
  , tags: map insensitive [ "Pure", "Functional", "Static", "Lazy" ]
  }

clojure :: Lang
clojure = Lang
  { key: Insensitive "clojure"
  , name: "Clojure"
  , description: "A dynamic programming language that targets the Java Virtual Machine"
  , homepage: "http://clojure.org/"
  , rating: fromNumber 14616
  , tags: map insensitive [ "Functional", "JVM", "Dynamic", "Lisp" ]
  }

befunge :: Lang
befunge = Lang
  { key: Insensitive "befunge"
  , name: "Befunge"
  , description: "A two-dimensional esoteric programming language"
  , homepage: "https://esolangs.org/wiki/Befunge"
  , rating: fromNumber 153
  , tags: map insensitive [ "2D", "Esoteric" ]
  }

idris :: Lang
idris = Lang
  { key: Insensitive "idris"
  , name: "Idris"
  , description: "Idris is a general purpose pure functional programming language with dependent types"
  , homepage: "http://www.idris-lang.org/"
  , rating: fromNumber 314
  , tags: map insensitive [ "Dependent", "Functional", "Static", "Pure" ]
  }
  
racket :: Lang
racket = Lang
  { key: Insensitive "racket"
  , name: "Racket"
  , description: "A programmable programming language"
  , homepage: "http://www.racket-lang.org/"
  , rating: fromNumber 1625
  , tags: map insensitive [ "Functional", "Lisp", "Dynamic", "Lazy" ]
  }

db :: DB
db = M.fromList $ map (\(l@(Lang o)) -> Tuple o.key l)
  [ purescript
  , haskell
  , clojure
  , befunge
  , idris
  , racket
  ]