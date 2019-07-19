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
import Budget.Component.HTML.Utils (OpaqueSlot)
import Budget.Data.Route (Route(..))
import Budget.Page.Admin as Admin
import Budget.Page.Dashboard as Dashboard
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Symbol (SProxy(..))
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap

type State = { route :: Route }

data Query a = Navigate Route a

data Action = EvalQuery (Query Unit)

type Input = Maybe Route

type ChildSlots =
  ( dashboardSlot :: OpaqueSlot Unit
  , adminSlot :: OpaqueSlot Unit
  )

_dashboardSlot = SProxy :: SProxy "dashboardSlot"
_adminSlot = SProxy :: SProxy "adminSlot"

component
  :: forall o m
   . MonadAff m
  => Now m
  => LogMessages m
  => Navigate m
  => ManageTemplate m
  => SendNotification m
  => ManageInstance m
  => H.Component HH.HTML Query Input o m
component = 
  H.mkComponent
    { initialState: \initialRoute -> { route: fromMaybe Dashboard initialRoute }
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, handleQuery = handleQuery }
    }
  where
  
  handleQuery :: forall a. Query a -> H.HalogenM State Action ChildSlots o m (Maybe a)
  handleQuery (Navigate dest a) = do
    { route } <- H.get
    when (route /= dest) do
      H.modify_ _ {route = dest}
    pure (Just a)

  handleAction :: Action -> H.HalogenM State Action ChildSlots o m Unit
  handleAction (EvalQuery q) = void $ handleQuery q

  render :: State -> HH.ComponentHTML Action ChildSlots m
  render { route } =   
    let Tuple pageContent pageTitle = case route of
          Dashboard ->
            Tuple (HH.slot _dashboardSlot unit Dashboard.component unit absurd) "Home"
          Admin ->
            Tuple (HH.slot _adminSlot unit Admin.component unit absurd) "Admin"
    in HH.div_ [ Navbar.navbar route               
               , HH.div [ HA.class_ Bootstrap.container ] 
                        [ pageHeader pageTitle
                        , pageContent 
                        ] 
               ]