module Budget.Data.Common where
  
import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromNumber, fromString)
import Data.DateTime (Date, date)
import Data.DateTime as F
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (Either)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat, unformatDateTime)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), (:))
import Data.Newtype (class Newtype)
  
newtype Currency = Currency Number

unCurrency :: Currency -> Number
unCurrency (Currency n) = n

derive instance eqCurrency :: Eq Currency
derive instance ordCurrency :: Ord Currency

derive instance genericCurrency :: Generic Currency _
derive instance newtypeCurrency :: Newtype Currency _

instance showCurrency:: Show Currency where
  show = genericShow

instance decodeJsonCurrency :: DecodeJson Currency where
  decodeJson json = Currency <$> decodeJson json

instance encodeJsonCurrency :: EncodeJson Currency where
  encodeJson (Currency num) = fromNumber num


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


newtype EndDate = EndDate Date

unEndDate :: EndDate -> Date
unEndDate (EndDate d) = d

derive instance genericEndDate :: Generic EndDate _
derive instance newtypeEndDate :: Newtype EndDate _
derive instance eqEndDate :: Eq EndDate
derive instance ordEndDate :: Ord EndDate


instance showEndDate :: Show EndDate where
  show = genericShow

instance decodeJsonEndDate :: DecodeJson EndDate where
  decodeJson json = 
    decodeJson json
      >>= map (EndDate <<< date) <<< unformatDateTime "YYYY-MM-DD"

instance encodeJsonEndDate :: EncodeJson EndDate where
  encodeJson (EndDate date) = 
      fromString 
    $ format conversionDateFormat
    $ toDateTime
    $ fromDate date

conversionDateFormat :: List FormatterCommand
conversionDateFormat = (YearFull:Placeholder "-":MonthTwoDigits:Placeholder "-":DayOfMonthTwoDigits:Nil) 

dateToString :: Date -> String
dateToString = format conversionDateFormat <<< toDateTime <<< fromDate

dateFromString :: String -> Either String Date
dateFromString = ((<$>) F.date) <<< unformat conversionDateFormat