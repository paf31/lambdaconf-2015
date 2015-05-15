-- | Functions for creating AJAX requests in the `Action` monad. 

module UI.AJAX
  ( AjaxAction()
  , listLangs
  , listTags
  , getLang
  , putLang
  , getTag
  ) where
    
import Data.Int
import Data.Maybe
import Data.Either
import Data.Foreign
import Data.Foreign.Class

import Control.Monad.Aff
import Control.Monad.Eff.Exception (Error(), error)

import Control.Monad.Error.Class (throwError)

import qualified Thermite.Action as T

import Network.HTTP.Affjax
import Network.HTTP.StatusCode (StatusCode(..))
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.RequestHeader (RequestHeader(..))
import Network.HTTP.Method (Method(..))
    
import UI.Types
import UI.Utils
    
-- | A type synonym for readability.
type AjaxAction eff response = forall state. T.Action (ajax :: AJAX | eff) state (Either String response)
    
ajax :: forall eff a. (IsForeign a) => Affjax eff Foreign -> AjaxAction eff a
ajax aff = T.async \k -> 
    runAff (\_ -> k (Left "Web service call failed"))
           (\r -> k (readWith (const "Cannot parse response") r))
           aff1
  where
  aff1 = do
    r <- aff
    case r.status of
      StatusCode n | n >= fromNumber 200 && n < fromNumber 300 -> return r.response
      _ -> throwError (error "Bad status code")
 
-- | List languages in the database.
listLangs :: forall eff. AjaxAction eff [LangSummary]
listLangs = ajax $ get "/api/lang/"
 
-- | List all available tags.
listTags :: forall eff. AjaxAction eff [TagSummary]
listTags = ajax $ get "/api/tag/"
 
-- | Get a language using its primary key.    
getLang :: forall eff. Key -> AjaxAction eff Lang
getLang key = ajax $ get $ "/api/lang/" <> key
      
-- | Insert or update a language.
putLang :: forall eff. Lang -> AjaxAction eff Ok
putLang lang = ajax $ affjax $ 
  defaultRequest { method = PUT
                 , url = "/api/lang/" <> (runLang lang).key
                 , content = Just (stringify lang)
                 , headers = [ContentType (MimeType "application/json")]
                 }

-- | Get a list of language summaries for a tag.             
getTag :: forall eff. Tag -> AjaxAction eff [LangSummary]
getTag tag = ajax <<< get $ "/api/tag/" <> tag

-- | TODO: Implement getPopular. Don't forget to export it!
-- | getPopular :: forall eff. AjaxAction eff [PopularLanguage]