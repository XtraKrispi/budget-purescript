module Budget.Component.HTML.Navbar where

import Prelude

import Budget.Component.HTML.Utils (safeHref)
import Budget.Data.Route (Route(..))
import Data.Tuple (Tuple(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap

navbar :: forall i p. Route -> HH.HTML i p
navbar route = 
  HH.nav [ HA.classes [ Bootstrap.navbar                      
                      , Bootstrap.navbarLight
                      , Bootstrap.navbarExpandLg
                      , Bootstrap.bgLight
                      ]  ] 
         [ HH.a [ HA.classes [ Bootstrap.navbarBrand ]
                , HA.href "#"] 
                [ HH.text "Budget"] 
         , HH.div [ HA.classes [ Bootstrap.collapse
                               , Bootstrap.navbarCollapse
                               ]
                  ] 
                  [ HH.ul [ HA.classes [ Bootstrap.navbarNav
                                       , Bootstrap.mrAuto
                                       ]
                          ]
                          (navItem route <$> routes)

                  ]
         ]
  where
    routes :: Array (Tuple Route String)
    routes = [ Tuple Dashboard "Home", Tuple Admin "Admin" ]

    navItem :: Route -> Tuple Route String -> HH.HTML i p
    navItem r (Tuple r' d) = 
      let active = if r == r' then [ Bootstrap.active] else []
      in  HH.li [ HA.classes ([ Bootstrap.navItem
                              ] <> active) 
                ] 
                [ HH.a [ HA.class_ Bootstrap.navLink
                       , safeHref r'
                       ] 
                       [ HH.text d 
                       ]
                ]