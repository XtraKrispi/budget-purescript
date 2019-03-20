module Budget.Page.Dashboard where

import Prelude

import Budget.Capability.Navigate (class Navigate)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH

type State = {}

data Query a = Initialize a

component
  :: forall m
   . MonadAff m
  => Navigate m
  => H.Component HH.HTML Query Unit Void m
component = 
  H.lifecycleComponent
    { initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just $ H.action Initialize 
    , finalizer: Nothing
    }
  where
  initialState :: Unit -> State
  initialState _ = {}

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      pure a

  render :: State -> H.ComponentHTML Query
  render _ = HH.div [] [ HH.text "Dashboard" ]