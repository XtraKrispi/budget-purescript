module Budget.Data.Common where
  
import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, fromNumber)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
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