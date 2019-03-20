module Budget.Component.HTML.Utils where

import Prelude

import Budget.Data.Common (Currency, unCurrency)
import Budget.Data.Route (Route, routeCodec)
import Data.Formatter.Number (Formatter(..), format)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Routing.Duplex (print)

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

formatCurrency :: Currency -> String
formatCurrency = append "$" 
       <<< format (Formatter { comma:  true, before: 0, after: 2, abbreviations: false, sign: false}) 
       <<< unCurrency
