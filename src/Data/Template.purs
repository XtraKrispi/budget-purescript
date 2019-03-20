module Budget.Data.Template where

import Prelude

import Budget.Data.Common (Currency)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fail, fromString, jsonEmptyObject, (.:))
import Data.Argonaut.Core (Json)
import Data.Date (Date)
import Data.DateTime (date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
import Data.Traversable (traverse)
import Data.Argonaut.Encode.Combinators ((:=), (~>))

data Frequency = OneTime | BiWeekly | Monthly

derive instance genericFrequency :: Generic Frequency _
derive instance eqFrequency :: Eq Frequency
derive instance ordFrequency :: Ord Frequency

instance showFrequency :: Show Frequency where
  show = genericShow

instance decodeJsonFrequency :: DecodeJson Frequency where
  decodeJson json = do
    str <- decodeJson json
    case str of
      "OneTime" -> pure OneTime
      "BiWeekly" -> pure BiWeekly
      "Monthly" -> pure Monthly
      _ -> fail "Invalid frequency"

instance encodeJsonFrequency :: EncodeJson Frequency where
  encodeJson = fromString <<< show

type TemplateRep row = 
  ( description :: String
  , amount      :: Currency
  , frequency   :: Frequency
  , startDate   :: StartDate
  , isDeleted   :: Boolean
  | row)

type Template t = { | TemplateRep t }

type TemplateWithKey = { templateId :: TemplateId | TemplateRep ()  }

newtype StartDate = StartDate Date

unStartDate :: StartDate -> Date
unStartDate (StartDate d) = d

derive instance genericStartDate :: Generic StartDate _
derive instance newtypeStartDate :: Newtype StartDate _
derive instance eqStartDate :: Eq StartDate
derive instance ordStartDate :: Ord StartDate


instance showStartDate :: Show StartDate where
  show = genericShow

instance decodeJsonStartDate :: DecodeJson StartDate where
  decodeJson json = 
    decodeJson json
      >>= map (StartDate <<< date) <<< unformatDateTime "YYYY-MM-DD"

instance encodeJsonStartDate :: EncodeJson StartDate where
  encodeJson (StartDate date) = 
      fromString 
    $ format (YearFull:Placeholder "-":MonthTwoDigits:Placeholder "-":DayOfMonthTwoDigits:Nil) 
    $ toDateTime 
    $ fromDate date

newtype TemplateId = TemplateId Int

derive instance eqTemplateId :: Eq TemplateId
derive instance genericTemplateId :: Generic TemplateId _
derive instance newtypeTemplateId :: Newtype TemplateId _

instance showTemplateId :: Show TemplateId where
  show = genericShow

instance decodeJsonTemplateId :: DecodeJson TemplateId where
  decodeJson json = TemplateId <$> decodeJson json

decodeTemplates :: Json -> Either String (Array TemplateWithKey)
decodeTemplates json = decodeJson json >>= traverse decodeTemplateWithKey

decodeTemplateWithKey :: Json -> Either String TemplateWithKey
decodeTemplateWithKey json = do
  obj <- decodeJson json
  tId <- TemplateId <$> obj .: "templateId"
  tObj <- obj .: "template"
  description <- tObj .: "description"
  amount <- tObj .: "amount"
  frequency <- tObj .: "frequency"
  startDate <- tObj .: "startDate"
  isDeleted <- tObj .: "isDeleted"
  pure { templateId: tId
       , description: description
       , amount: amount
       , frequency: frequency
       , startDate: startDate
       , isDeleted: isDeleted
       }

encodeTemplate :: forall t. Template t -> Json
encodeTemplate { description, amount, frequency, startDate, isDeleted } = 
     "description" := description
  ~> "amount" := amount
  ~> "frequency" := frequency
  ~> "startDate" := startDate
  ~> "isDeleted" := isDeleted
  ~> jsonEmptyObject