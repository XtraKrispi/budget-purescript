module Budget.Api.Endpoint where

import Prelude hiding ((/))

import Budget.Data.Template (TemplateId)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Lens.Iso.Newtype (_Newtype)
import Routing.Duplex (RouteDuplex', int, root, segment)
import Routing.Duplex.Generic (noArgs, sum)
import Routing.Duplex.Generic.Syntax ((/))

data Endpoint
  = Template TemplateId
  | Templates 

derive instance genericEndpoint :: Generic Endpoint _

instance showEndpoint :: Show Endpoint where
  show = genericShow

endpointCodec :: RouteDuplex' Endpoint
endpointCodec = root $ sum
  { "Template": "templates" / templateId
  , "Templates": "templates" / noArgs
  }

templateId :: RouteDuplex' TemplateId
templateId = _Newtype (int segment)