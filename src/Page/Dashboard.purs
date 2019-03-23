module Budget.Page.Dashboard where

import Prelude

import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now, nowDate)
import Budget.Capability.Resource.Instance (class ManageInstance, createInstance, getInstances)
import Budget.Capability.SendNotification (class SendNotification, sendErrorNotification)
import Budget.Component.HTML.Utils (actionIconButton, formatCurrency)
import Budget.Data.Common (Currency(..), EndDate(..), conversionDateFormat, unCurrency, unEndDate)
import Budget.Data.Instance (Instance, InstanceType(..))
import Budget.Page.Admin (dateFormat)
import DOM.HTML.Indexed.InputType (InputType(..))
import Data.Array (elem, filter)
import Data.Date (Date, adjust)
import Data.DateTime (date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either, hush)
import Data.Foldable (sum)
import Data.Formatter.DateTime (format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Number (fromString)
import Data.Time.Duration (Days(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (get)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..), checked, class_, classes, name, type_, value, readOnly) as HA
import Halogen.Themes.Bootstrap4 as Bootstrap
import Halogen.Themes.Bootstrap4 as Bootstrap
import Halogen.Themes.Bootstrap4 as Bootstrap
import Network.RemoteData (RemoteData(..), fromEither)

type State = 
  { instances :: RemoteData String (Array Instance)
  , endDate :: Maybe EndDate
  , currentFilter :: Filter
  , scratchAmount :: Currency
  }

data Filter = NotActionedFilter | AllFilter | ActionedFilter | CompletedFilter | SkippedFilter 

derive instance eqFilter :: Eq Filter
derive instance genericFilter :: Generic Filter _

instance showFilter :: Show Filter where
  show = genericShow

data Query a = 
    Initialize a
  | LoadInstances EndDate a
  | PayInstance Instance a 
  | SkipInstance Instance a
  | ApplyFilter Filter a
  | EndDateChanged String a
  | ScratchAmountChanged String a


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
  initialState _ = { instances: NotAsked
                   , endDate: Nothing
                   , currentFilter: NotActionedFilter 
                   , scratchAmount: Currency 0.0
                   }      

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      endDate <- defaultEndDate
      void $ H.fork $ eval $ LoadInstances endDate a
      pure a
      
    LoadInstances eDate a -> do
      H.modify_ _{ instances = Loading, endDate = Just eDate }
      instances <- fromEither <$> getInstances eDate
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

    EndDateChanged s a -> do
      newEndDate <- maybe defaultEndDate pure (de s)
      void $ H.fork $ eval $ LoadInstances newEndDate a
      pure a

    ScratchAmountChanged s a -> do
      maybe (pure unit) (\amt' -> H.modify_ _{ scratchAmount = Currency amt' }) (fromString s)
      pure a

defaultEndDate :: forall m. Now m => m EndDate
defaultEndDate = do
  today <- nowDate
  let adjusted = EndDate $ fromMaybe today $ adjust (Days 21.0) today
  pure adjusted


render :: State -> H.ComponentHTML Query
render s = 
  HH.div [ HA.class_ Bootstrap.row ] 
  [ HH.div [ HA.class_ Bootstrap.col ] 
    [ renderInstances s ]
  , HH.div [ HA.class_ Bootstrap.col ] 
    [ scratchArea s ] 
  , HH.div [] [ HH.text $ show s]
  ]

renderInstances :: State -> H.ComponentHTML Query
renderInstances { instances, endDate, currentFilter } =
  case instances of
    NotAsked -> HH.div [] [ HH.text "No data loaded." ]
    Loading  -> HH.div [] [ HH.text "Loading..." ]
    Failure err -> HH.div [] [ HH.text $ "Error: " <> err ]
    Success i ->
      HH.div [] 
      [ HH.div [ HA.class_ Bootstrap.formGroup ] 
        [ HH.label_ [ HH.text "Instances ending: " ]
        , HH.input [ HA.type_ HA.InputDate
                   , HA.classes [ Bootstrap.formControl ] 
                   , HA.value $ fromMaybe "" $ ed <$> endDate
                   , HE.onValueInput (HE.input EndDateChanged)
                   ]
        ]    
      , HH.div [ HA.classes [ Bootstrap.btnGroup, Bootstrap.btnGroupToggle, Bootstrap.mb2 ]] 
        [ filterButton "Not Actioned" NotActionedFilter
        , filterButton "All" AllFilter
        , filterButton "Actioned" ActionedFilter
        , filterButton "Completed" CompletedFilter    
        , filterButton "Skipped" SkippedFilter
        ]
      , HH.div [] $ renderInstance <$> filtered i
      ]
      where 
        filterMap NotActionedFilter = [ NotActioned ]
        filterMap AllFilter = [ NotActioned, Completed, Skipped ]
        filterMap ActionedFilter = [ Completed, Skipped ]
        filterMap CompletedFilter = [ Completed ]
        filterMap SkippedFilter = [ Skipped ]

        filtered = filter (\{ instanceType } -> instanceType `elem` filterMap currentFilter )
        filterButton text filter = 
          HH.label [ HA.classes $ [ Bootstrap.btn, Bootstrap.btnLight ] <> (if active then [Bootstrap.active] else [])] 
          [ HH.input [ HA.type_ InputRadio
                     , HA.name "filters"
                     , HA.checked active
                     , HE.onChecked (HE.input_ $ ApplyFilter filter)] 
          , HH.text text
          ] 
          where active = currentFilter == filter

ed :: EndDate -> String
ed = format conversionDateFormat <<< toDateTime <<< fromDate <<< unEndDate

de :: String -> Maybe EndDate
de = hush <<< ((<$>) (EndDate <<< date)) <<< unformat conversionDateFormat

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

scratchArea :: forall r. { scratchAmount :: Currency, instances :: RemoteData String (Array Instance) | r } -> H.ComponentHTML Query
scratchArea { scratchAmount, instances } = 
  case instances of
    Success i -> 
      HH.div_ 
      [ totalRow i
      , userAmountRow  
      , remainingRow 
      ]                      
    _         -> HH.div [] 
                 [ HH.div [ HA.class_ Bootstrap.formGroup ] 
                   [ HH.label_ [ HH.text "Total"]
                   , HH.input [ HA.class_ Bootstrap.formControl ]
                   ]
                 ]
  where 
    unactioned :: Array Instance -> Array Instance
    unactioned = filter ((==) NotActioned <<< _.instanceType)
    total :: Array Instance -> Currency
    total = sum <<< map _.amount

    row = HH.div [ HA.classes [ Bootstrap.formGroup, Bootstrap.row]]

    totalRow i = 
      row [ HH.label [ HA.class_ Bootstrap.colSm3 ] 
            [ HH.text "Total"]
          , HH.input [ HA.readOnly true
                     , HA.classes [ Bootstrap.formControl
                                  , Bootstrap.colSm4 ]
                     , HA.value (formatCurrency $ total $ unactioned i)]]
    userAmountRow =
      row [ HH.label [HA.class_ Bootstrap.colSm3 ] 
              [ HH.text "In Account"]
          , HH.div [ HA.classes [ Bootstrap.inputGroup
                                , Bootstrap.mb2
                                , Bootstrap.mr2
                                , Bootstrap.colSm4 ]]
            [ HH.div [ HA.class_ Bootstrap.inputGroupPrepend ] 
              [ HH.div [ HA.class_ Bootstrap.inputGroupText ]
                [ HH.text "$" ]]
            , HH.input [ HA.type_ InputNumber
                       , HA.value (show $ unCurrency scratchAmount)
                       , HA.classes [ Bootstrap.formControl ]
                       , HE.onValueInput (HE.input ScratchAmountChanged)]]]
    remainingRow = 
      row [ HH.label [HA.class_ Bootstrap.colSm3 ] 
            [ HH.text "Left Over"] 
          , HH.input [ HA.readOnly true
                     , HA.classes [ Bootstrap.formControl
                                  , Bootstrap.colSm4 ]]]

      