module Budget.Page.Dashboard where

import Prelude

import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now, nowDate)
import Budget.Capability.Resource.Instance (class ManageInstance, createInstance, getInstances)
import Budget.Capability.SendNotification (class SendNotification, sendErrorNotification)
import Budget.Component.HTML.Utils (actionIconButton, formatCurrency)
import Budget.Data.Common (EndDate(..))
import Budget.Data.Instance (Instance, InstanceType(..))
import Budget.Page.Admin (dateFormat)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (elem, filter)
import Data.Date (Date, adjust)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either)
import Data.Formatter.DateTime (format)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Time.Duration (Days(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (get)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Bootstrap
import Network.RemoteData (RemoteData(..), fromEither)

type State = 
  { instances :: RemoteData String (Array Instance)
  , endDate :: Maybe EndDate
  , currentFilter :: Filter
  }

data Filter = NotActionedFilter | AllFilter | ActionedFilter | CompletedFilter | SkippedFilter 

derive instance eqFilter :: Eq Filter

data Query a = 
    Initialize a
  | LoadInstances EndDate a
  | PayInstance Instance a 
  | SkipInstance Instance a
  | ApplyFilter Filter a


component
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageInstance m
  => SendNotification m
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
  initialState _ = { instances: NotAsked, endDate: Nothing, currentFilter: NotActionedFilter }      

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

    PayInstance i a -> do      
      { instances } <- get
      let updatedInstance = i{instanceType = Completed}
      createInstance updatedInstance >>=
        either (\err -> sendErrorNotification "There was a problem with the operation. Please try again.")
               (\_   -> 
                H.modify_ _{ instances = map (\i' -> if i == i' then updatedInstance else i')  <$> instances }
               )      
      pure a

    SkipInstance i a -> do
      { instances } <- get
      let updatedInstance = i{instanceType = Skipped}
      createInstance updatedInstance >>=
        either (\err -> sendErrorNotification "There was a problem with the operation. Please try again.")
               (\_   -> 
                H.modify_ _{ instances = map (\i' -> if i == i' then updatedInstance else i')  <$> instances }
               )      
      pure a

    ApplyFilter f a -> do
      H.modify_ _{ currentFilter = f}
      pure a

defaultEndDate :: forall m. Now m => m EndDate
defaultEndDate = do
  today <- nowDate
  let adjusted = EndDate $ fromMaybe today $ adjust (Days 21.0) today
  pure adjusted


render :: State -> H.ComponentHTML Query
render { instances, currentFilter } = 
  HH.div [ HA.class_ Bootstrap.row ] 
  [ HH.div [ HA.class_ Bootstrap.col ] 
    [ renderInstances currentFilter instances ]
  , HH.div [ HA.class_ Bootstrap.col ] 
    [ HH.text "Scratch Area goes here" ] ]

renderInstances :: Filter -> RemoteData String (Array Instance) -> H.ComponentHTML Query
renderInstances _ NotAsked = HH.div [] [ HH.text "No data loaded." ]
renderInstances _ Loading  = HH.div [] [ HH.text "Loading..." ]
renderInstances _ (Failure err) = HH.div [] [ HH.text $ "Error: " <> err ]
renderInstances currentFilter (Success instances) = 
  HH.div [] [ 
     HH.div [ HA.classes [ Bootstrap.btnGroup, Bootstrap.btnGroupToggle, Bootstrap.mb2 ]] 
     [ filterButton "Not Actioned" NotActionedFilter
     , filterButton "All" AllFilter
     , filterButton "Actioned" ActionedFilter
     , filterButton "Completed" CompletedFilter    
     , filterButton "Skipped" SkippedFilter
     ]
   , HH.div [] $ renderInstance <$> filtered
  ]
  where 
    filterMap NotActionedFilter = [ NotActioned ]
    filterMap AllFilter = [ NotActioned, Completed, Skipped ]
    filterMap ActionedFilter = [ Completed, Skipped ]
    filterMap CompletedFilter = [ Completed ]
    filterMap SkippedFilter = [ Skipped ]

    filtered = filter (\{ instanceType } -> instanceType `elem` filterMap currentFilter ) instances
    filterButton text filter = 
      HH.label [ HA.classes $ [ Bootstrap.btn, Bootstrap.btnSecondary ] <> (if active then [Bootstrap.active] else [])] 
      [ HH.input [ HA.type_ InputRadio
                 , HA.name "filters"
                 , HA.checked active
                 , HE.onChecked (HE.input_ $ ApplyFilter filter)] 
      , HH.text text
      ] 
      where active = currentFilter == filter


prettyDate :: Date -> String
prettyDate = format dateFormat <<< toDateTime <<< fromDate

renderInstance :: Instance -> H.ComponentHTML Query
renderInstance i =
  HH.div [ HA.classes [ Bootstrap.card, Bootstrap.mb5, Bootstrap.bgLight ] ] 
  [ HH.div [ HA.class_ Bootstrap.cardHeader ] 
    [ HH.span_ [ HH.text i.description ] 
    , HH.span [ HA.class_ Bootstrap.floatRight ] 
      [ actionIconButton "far fa-check-circle" (PayInstance i) 
      , actionIconButton "far fa-times-circle" (SkipInstance i) 
      ]
    ]
  , HH.div [ HA.class_ Bootstrap.cardBody ]
    [ HH.h5 [ HA.class_ Bootstrap.cardTitle ] 
      [ HH.text $ formatCurrency i.amount ]
    , HH.h6 [ HA.classes [ Bootstrap.cardSubtitle, Bootstrap.mb2, Bootstrap.textMuted ]]
      [ HH.span_ 
        [ HH.text $ prettyDate i.date ]
      ]
    ]
  ]

