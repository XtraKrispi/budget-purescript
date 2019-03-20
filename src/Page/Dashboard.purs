module Budget.Page.Dashboard where

import Prelude

import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now, nowDate)
import Budget.Capability.Resource.Instance (class ManageInstance, getInstances)
import Budget.Component.HTML.Utils (formatCurrency)
import Budget.Data.Common (EndDate(..))
import Budget.Data.Instance (Instance)
import Budget.Page.Admin (dateFormat)
import Data.Date (Date, adjust)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Formatter.DateTime (format)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap
import Halogen.Themes.Bootstrap4 as Bootstrap
import Halogen.Themes.Bootstrap4 as Bootstrap
import Network.RemoteData (RemoteData(..), fromEither)

type State = 
  { instances :: RemoteData String (Array Instance)
  , endDate :: Maybe EndDate
  }

data Query a = 
   Initialize a
  |LoadInstances EndDate a


component
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageInstance m
  => Now m
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
  initialState _ = { instances: NotAsked, endDate: Nothing }      

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      endDate <- defaultEndDate
      void $ H.fork $ eval $ LoadInstances endDate a
      pure a
      
    LoadInstances ed a -> do
      H.modify_ _{ instances = Loading, endDate = Just ed }
      instances <- fromEither <$> getInstances ed
      H.modify_ _{ instances = instances }
      pure a

defaultEndDate :: forall m. Now m => m EndDate
defaultEndDate = do
  today <- nowDate
  let adjusted = EndDate $ fromMaybe today $ adjust (Days 21.0) today
  pure adjusted


render :: State -> H.ComponentHTML Query
render { instances } = 
  HH.div [ HA.class_ Bootstrap.row ] 
  [ HH.div [ HA.class_ Bootstrap.col ] 
    [ renderInstances instances ]
  , HH.div [ HA.class_ Bootstrap.col ] 
    [ HH.text "Scratch Area goes here" ] ]

renderInstances :: RemoteData String (Array Instance) -> H.ComponentHTML Query
renderInstances NotAsked = HH.div [] [ HH.text "No data loaded." ]
renderInstances Loading  = HH.div [] [ HH.text "Loading..." ]
renderInstances (Failure err) = HH.div [] [ HH.text $ "Error: " <> err ]
renderInstances (Success instances) = 
  HH.div [] $ renderInstance <$> instances


prettyDate :: Date -> String
prettyDate = format dateFormat <<< toDateTime <<< fromDate

renderInstance :: Instance -> H.ComponentHTML Query
renderInstance i =
  HH.div [ HA.classes [ Bootstrap.card, Bootstrap.mb5, Bootstrap.bgLight ] ] 
  [ HH.div [ HA.class_ Bootstrap.cardHeader ] [ HH.text i.description ]
  , HH.div [ HA.class_ Bootstrap.cardBody ]
    [ HH.h5 [ HA.class_ Bootstrap.cardTitle ] [ HH.text $ formatCurrency i.amount ]
  , HH.h6 [ HA.classes [ Bootstrap.cardSubtitle, Bootstrap.mb2, Bootstrap.textMuted ]]
    [ HH.text $ prettyDate i.date ]]]
