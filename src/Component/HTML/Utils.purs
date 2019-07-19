module Budget.Component.HTML.Utils where

import Prelude

import Budget.Data.Common (Currency, unCurrency)
import Budget.Data.Route (Route, routeCodec)
import Data.Const (Const)
import Data.Formatter.Number (Formatter(..), format)
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.HTML.Properties as HP
import Halogen.Themes.Bootstrap4 as Bootstrap
import Routing.Duplex (print)

type OpaqueSlot = H.Slot (Const Void) Void

safeHref :: forall r i. Route -> HH.IProp ( href :: String | r) i
safeHref = HP.href <<< append "#" <<< print routeCodec

formatCurrency :: Currency -> String
formatCurrency = append "$" 
       <<< format (Formatter { comma:  true, before: 0, after: 2, abbreviations: false, sign: false}) 
       <<< unCurrency

actionIconButton :: forall i a. String -> a -> HH.HTML i a
actionIconButton c a = 
  HH.button [ HA.classes [ Bootstrap.btn, Bootstrap.btnSm, Bootstrap.bgTransparent ]
            , HE.onClick (const $ Just a)
            ] 
  [ HH.i [ HA.class_ (H.ClassName c) ] []
  ]

