module Main where

import Debug.Trace

import Data.Int
import Data.Maybe
import Data.Tuple

import qualified Data.Map as M

import Control.Monad.Eff
import Control.Monad.Eff.Ref
import Control.Monad.Eff.Exception
import Control.Monad.Eff.Class

import Node.Express.Types
import Node.Express.App
import Node.Express.Handler

type Key = String

newtype Lang = Lang
  { name :: String
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [String]
  }

type DB = M.Map Key Lang

purescript :: Lang
purescript = Lang
  { name: "PureScript"
  , description: "A small strongly typed programming language that compiles to JavaScript"
  , homepage: "http://purescript.org/"
  , rating: zero
  , tags: [ "Pure", "Functional", "Static", "AltJS" ]
  }
  
haskell :: Lang
haskell = Lang
  { name: "Haskell"
  , description: "An advanced purely-functional programming language"
  , homepage: "http://haskell.org/"
  , rating: zero
  , tags: [ "Pure", "Functional", "Static", "Lazy" ]
  }

clojure :: Lang
clojure = Lang
  { name: "Clojure"
  , description: "A dynamic programming language that targets the Java Virtual Machine"
  , homepage: "http://clojure.org/"
  , rating: zero
  , tags: [ "Functional", "JVM", "Dynamic", "Lisp" ]
  }

befunge :: Lang
befunge = Lang
  { name: "Befunge"
  , description: "A two-dimensional esoteric programming language"
  , homepage: "https://esolangs.org/wiki/Befunge"
  , rating: zero
  , tags: [ "2D", "Esoteric" ]
  }

initialDb :: DB
initialDb = M.fromList 
  [ Tuple "purescript" purescript
  , Tuple "haskell" haskell
  , Tuple "clojure" clojure
  , Tuple "befunge" befunge
  ]

indexHandler :: Handler
indexHandler = send 
  { name: "langdb server"
  , follow: "http://localhost:9000/lang"
  }

listHandler :: RefVal DB -> Handler
listHandler db = do
  m <- liftEff $ readRef db
  send $ fromTuple <$> M.toList m
  where
  fromTuple (Tuple key (Lang o)) = { key: key, name: o.name, uri: "http://localhost:9000/lang/" <> key }

getHandler :: RefVal DB -> Handler
getHandler db = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow (error "id parameter is required") 
    Just _id -> do
      m <- liftEff $ readRef db
      case M.lookup _id m of
        Nothing -> do
          setStatus 404
          send "Not found"
        Just val -> send val

app :: RefVal DB -> App
app db = do
  setProp "json spaces" 2
  get "/" indexHandler
  get "/lang/" (listHandler db)
  get "/lang/:id" (getHandler db)

main = do
  db <- newRef initialDb
  listenHttp (app db) 9000 \_ -> trace "Listening on port 9000"
