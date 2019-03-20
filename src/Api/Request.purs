module Budget.Api.Request where
  
import Prelude

import Affjax (Request)
import Affjax.RequestBody as RB
import Affjax.ResponseFormat as RF
import Budget.Api.Endpoint (Endpoint, endpointCodec)
import Data.Argonaut (Json)
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML (b)
import Routing.Duplex (print)
import Web.HTML.HTMLDocument (body)
import Web.XHR.XMLHttpRequest (withCredentials)
  
newtype BaseURL = BaseURL String

derive instance genericBaseUrl :: Generic BaseURL _

instance showBaseUrl :: Show BaseURL where
  show = genericShow

data RequestMethod
  = Get
  | Post (Maybe Json)
  | Put  (Maybe Json)
  | Delete

type RequestOptions = 
  { endpoint :: Endpoint
  , method   :: RequestMethod
  }

defaultRequest :: BaseURL -> RequestOptions -> Request Json
defaultRequest (BaseURL baseUrl) { endpoint, method } = 
  { method: Left method
  , url: baseUrl <> print endpointCodec endpoint
  , headers: []
  , content: RB.json <$> body
  , username: Nothing
  , password: Nothing
  , withCredentials: false
  , responseFormat: RF.json
  }
  where
  Tuple method body = case method of
    Get -> Tuple GET Nothing
    Post b -> Tuple POST b
    Put b -> Tuple PUT b
    Delete -> Tuple DELETE Nothing