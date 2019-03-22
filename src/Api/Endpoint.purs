module Budget.Api.Endpoint where

import Prelude hiding ((/))

import Budget.Data.Common (EndDate, dateFromString, dateToString)
import Budget.Data.Template (TemplateId)
import Data.Date (Date)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', int, root, segment, as)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Template TemplateId
  | Templates
  | Instances EndDate
  | CreateInstance
  | DeleteInstance TemplateId Date

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Template": "templates" / templateId
  , "Templates": "templates" / noArgs
  , "Instances": "instances" / endDate
  , "CreateInstance": "instances" / noArgs
  , "DeleteInstance":  "instances" / templateId / (date segment)
  }

templateId :: RouteDuplex' TemplateId
templateId = _Newtype (int segment)

endDate :: RouteDuplex' EndDate
endDate = _Newtype (date segment)

date :: RouteDuplex' String -> RouteDuplex' Date
date = as dateToString dateFromString