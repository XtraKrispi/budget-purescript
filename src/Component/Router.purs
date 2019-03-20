module Budget.Component.Router where

import Prelude

import Budget.Capability.LogMessages (class LogMessages)
import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now)
import Budget.Capability.Resource.Instance (class ManageInstance)
import Budget.Capability.Resource.Template (class ManageTemplate)
import Budget.Capability.SendNotification (class SendNotification)
import Budget.Component.HTML.Navbar as Navbar
import Budget.Component.HTML.PageHeader (pageHeader)
import Budget.Data.Route (Route(..))
import Budget.Page.Admin as Admin
import Budget.Page.Dashboard as Dashboard
import Data.Either.Nested (Either2)
import Data.Functor.Coproduct.Nested (Coproduct2)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap

type State = { route :: Route }

data Query a = Navigate Route a

type Input = Maybe Route

type ChildQuery = Coproduct2 Dashboard.Query Admin.Query

type ChildSlot = Either2 Unit Unit

component
  :: forall m
   . MonadAff m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageTemplate m
  => SendNotification m
  => ManageInstance m
  => H.Component HH.HTML Query Input Void m
component = 
  H.parentComponent
    { initialState: \initialRoute -> { route: fromMaybe Dashboard initialRoute }
    , render
    , eval
    , receiver: const Nothing
    }
  where
  
  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void m
  eval (Navigate dest a) = do
    { route } <- H.get
    when (route /= dest) do
      H.modify_ _ {route = dest}
    pure a

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot m
  render { route } =   
    let Tuple pageContent pageTitle = case route of
          Dashboard ->
            Tuple (HH.slot' CP.cp1 unit Dashboard.component unit absurd) "Home"
          Admin ->
            Tuple (HH.slot' CP.cp2 unit Admin.component unit absurd) "Admin"
    in HH.div_ [ Navbar.navbar route               
               , HH.div [ HA.class_ Bootstrap.container ] 
                        [ pageHeader pageTitle
                        , pageContent 
                        ] 
               ]