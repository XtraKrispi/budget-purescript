module Budget.Component.HTML.PageHeader where

import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap

pageHeader :: forall i p. String -> HH.HTML i p
pageHeader t = 
  HH.header 
    [ HA.classes [ Bootstrap.mt5
                 , Bootstrap.mb5 
                 ] 
    ] 
    [ HH.h1_ [ HH.text t 
            ] 
    ]