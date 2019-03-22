module Budget.Data.Instance where

import Prelude

import Budget.Data.Common (Currency, dateFromString, dateToString)
import Budget.Data.Template (TemplateId(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fail, jsonEmptyObject, jsonNull, (.:))
import Data.Argonaut.Encode.Combinators ((:=), (~>))
import Data.Date (Date)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Traversable (traverse)

type Instance = 
  { originalTemplateId :: TemplateId
  , description :: String
  , amount :: Currency
  , instanceType :: InstanceType
  , date :: Date
  }

data InstanceType = NotActioned | Completed | Skipped

derive instance eqInstanceType :: Eq InstanceType
derive instance ordInstanceType :: Ord InstanceType
derive instance genericInstanceType :: Generic InstanceType _

instance showInstanceType :: Show InstanceType where
  show = genericShow

instance decodeJsonInstanceType :: DecodeJson InstanceType where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "NotActioned" -> pure NotActioned
      "Completed" -> pure Completed
      "Skipped" -> pure Skipped
      _ -> fail "Invalid instance type" 

instance encodeJsonInstanceType :: EncodeJson InstanceType where
  encodeJson = encodeJson <<< show

decodeInstances :: Json -> Either String (Array Instance)
decodeInstances json = decodeJson json >>= traverse decodeInstance

decodeInstance :: Json -> Either String Instance
decodeInstance json = do
  obj <- decodeJson json
  tId <- TemplateId <$> obj .: "originalTemplateId"
  description <- obj .: "description"
  amount <- obj .: "amount"
  t <- obj .: "type"
  date <- (obj .: "date") >>= dateFromString
  pure { originalTemplateId: tId
       , description
       , amount
       , instanceType: t
       , date
       }

encodeDate :: Date -> Json
encodeDate = encodeJson <<< dateToString

encodeInstance :: Instance -> Json
encodeInstance { originalTemplateId, description, amount, instanceType, date } = 
     "originalTemplateId" := originalTemplateId
  ~> "description" := description
  ~> "amount" := amount
  ~> "type" := instanceType
  ~> "date" := encodeDate date
  ~> jsonEmptyObject