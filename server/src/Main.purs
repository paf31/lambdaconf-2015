module Main where

import Debug.Trace

import Data.Int
import Data.Maybe
import Data.Tuple
import Data.Either
import Data.Array (filter, map, nub, sort)
import Data.Foldable (elem)

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
  { key :: Key
  , name :: String
  , description :: String
  , homepage :: String
  , rating :: Int
  , tags :: [String]
  }
  
runLang (Lang lang) = lang

type DB = M.Map Key Lang

-- Ratings based on number of GitHub search results

purescript :: Lang
purescript = Lang
  { key: "purescript"
  , name: "PureScript"
  , description: "A small strongly typed programming language that compiles to JavaScript"
  , homepage: "http://purescript.org/"
  , rating: fromNumber 463
  , tags: [ "Pure", "Functional", "Static", "AltJS" ]
  }
  
haskell :: Lang
haskell = Lang
  { key: "haskell"
  , name: "Haskell"
  , description: "An advanced purely-functional programming language"
  , homepage: "http://haskell.org/"
  , rating: fromNumber 16586
  , tags: [ "Pure", "Functional", "Static", "Lazy" ]
  }

clojure :: Lang
clojure = Lang
  { key: "clojure"
  , name: "Clojure"
  , description: "A dynamic programming language that targets the Java Virtual Machine"
  , homepage: "http://clojure.org/"
  , rating: fromNumber 14616
  , tags: [ "Functional", "JVM", "Dynamic", "Lisp" ]
  }

befunge :: Lang
befunge = Lang
  { key: "befunge"
  , name: "Befunge"
  , description: "A two-dimensional esoteric programming language"
  , homepage: "https://esolangs.org/wiki/Befunge"
  , rating: fromNumber 153
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
  , lang: "http://localhost:9000/api/lang"
  , tag: "http://localhost:9000/api/tag"
  }
 
shortLang :: Tuple Key Lang -> { key :: Key, name :: String, uri :: String, like :: String, dislike :: String }
shortLang (Tuple key (Lang o)) = 
  { key: key
  , name: o.name
  , uri: "http://localhost:9000/api/lang/" <> key 
  , like: "http://localhost:9000/api/lang/" <> key <> "/like"
  , dislike: "http://localhost:9000/api/lang/" <> key <> "/dislike"
  }

listHandler :: RefVal DB -> Handler
listHandler db = do
  m <- liftEff $ readRef db
  send <<< map shortLang <<< M.toList $ m
  where
  fromTuple (Tuple key (Lang o)) = { key: key, name: o.name, uri: "http://localhost:9000/api/lang/" <> key }

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
        
rateHandler :: RefVal DB -> Handler
rateHandler db = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow (error "id parameter is required") 
    Just _id -> do
      m <- liftEff $ readRef db
      case M.lookup _id m of
        Nothing -> do
          setStatus 404
          send "Not found"
        Just _ -> do
          liftEff $ modifyRef db $ M.update (\(Lang lang) -> Just $ Lang (lang { rating = one + lang.rating })) _id
          send "ok"
        
putHandler :: RefVal DB -> Handler
putHandler db = do
  idParam <- getRouteParam "id"
  case idParam of
    Nothing -> nextThrow (error "id parameter is required") 
    Just _id -> do
      name <- getBodyParam "name"
      description <- getBodyParam "description"
      homepage <- getBodyParam "homepage"
      tags <- getBodyParam "tags"
      let lang = { key: _id
                 , name: _
                 , description: _
                 , homepage: _
                 , rating: zero :: Int
                 , tags: _
                 } <$> (name         `orDie` "Name is required")
                   <*> (description  `orDie` "Description is required")
                   <*> (homepage     `orDie` "Homepage is required")
                   <*> (tags         `orDie` "Tags are required")
      case lang of
        Left err -> do
          setStatus 406
          send err
        Right lang -> do
          liftEff $ modifyRef db $ M.insert _id $ Lang lang
          send "ok"
  where
  orDie :: forall a. Maybe a -> String -> Either String a
  orDie Nothing s = Left s
  orDie (Just a) _ = Right a
        
tagsHandler :: RefVal DB -> Handler
tagsHandler db = do
  m <- liftEff $ readRef db
  send <<< map makeEntry <<< sort <<< nub $ do
    lang <- M.values m
    (runLang lang).tags
  where
  makeEntry tag = { tag: tag, uri: "http://localhost:9000/api/tag/" <> tag }
  
tagHandler :: RefVal DB -> Handler
tagHandler db = do
  tagParam <- getRouteParam "tag"
  case tagParam of
    Nothing -> nextThrow (error "tag parameter is required") 
    Just _tag -> do
      m <- liftEff $ readRef db
      send <<< map shortLang <<< filter (hasTag _tag) <<< M.toList $ m
  where
  hasTag t (Tuple _ (Lang o)) = t `elem` o.tags 

foreign import jsonBodyParser "var jsonBodyParser = require('body-parser').json()" :: forall middleware. middleware

foreign import staticFiles "var staticFiles = require('express').static('../client')" :: forall middleware. middleware

app :: RefVal DB -> App
app db = do
  setProp "json spaces" 2
  
  useExternal jsonBodyParser
  useExternal staticFiles
  
  get "/api" indexHandler
  
  get "/api/lang" (listHandler db)
  get "/api/lang/:id" (getHandler db)
  put "/api/lang/:id" (putHandler db)

  post "/api/lang/:id/like" (rateHandler db)
  
  get "/api/tag" (tagsHandler db)
  get "/api/tag/:tag" (tagHandler db)

main = do
  db <- newRef initialDb
  listenHttp (app db) 9000 \_ -> trace "Listening on port 9000"
