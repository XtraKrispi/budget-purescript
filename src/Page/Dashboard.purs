module Budget.Page.Dashboard where

import Prelude

import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now, nowDate)
import Budget.Capability.Resource.Instance (class ManageInstance, createInstance, getInstances)
import Budget.Capability.SendNotification (class SendNotification, sendErrorNotification)
import Budget.Component.HTML.Utils (actionIconButton, formatCurrency)
import Budget.Data.Common (Currency(..), EndDate(..), conversionDateFormat, unEndDate)
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
import Network.RemoteData (RemoteData(..), fromEither)

type State = 
  { instances :: RemoteData String (Array Instance)
  , endDate :: Maybe EndDate
  , currentFilter :: Filter
  , scratchAmount :: ScratchAmount
  }

data ScratchAmount = ScratchAmount Currency String

derive instance genericScratchAmount :: Generic ScratchAmount _

instance showScratchAmount :: Show ScratchAmount where
  show = genericShow

data Filter = NotActionedFilter | AllFilter | ActionedFilter | CompletedFilter | SkippedFilter 

derive instance eqFilter :: Eq Filter
derive instance genericFilter :: Generic Filter _

instance showFilter :: Show Filter where
  show = genericShow

data Action = 
    Initialize
  | LoadInstances EndDate
  | PayInstance Instance 
  | SkipInstance Instance
  | ApplyFilter Filter
  | EndDateChanged String
  | ScratchAmountChanged String


component
  :: forall q i o m
   . MonadAff m
  => Navigate m
  => ManageInstance m
  => SendNotification m
  => Now m
  => H.Component HH.HTML q i o m
component = 
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction, initialize = Just Initialize }
    }
  where
  initialState :: i -> State
  initialState _ = { instances: NotAsked
                   , endDate: Nothing
                   , currentFilter: NotActionedFilter 
                   , scratchAmount: ScratchAmount (Currency 0.0) ""
                   }      

  handleAction :: Action -> H.HalogenM State Action () o m Unit
  handleAction = case _ of
    Initialize -> do
      endDate <- defaultEndDate
      void $ H.fork $ handleAction $ LoadInstances endDate
      
    LoadInstances eDate -> do
      H.modify_ _{ instances = Loading, endDate = Just eDate }
      instances <- fromEither <$> getInstances eDate
      H.modify_ _{ instances = instances }

    PayInstance i -> do      
      { instances } <- get
      let updatedInstance = i{instanceType = Completed}
      createInstance updatedInstance >>=
        either (\err -> sendErrorNotification "There was a problem with the operation. Please try again.")
               (\_   -> 
                H.modify_ _{ instances = map (\i' -> if i == i' then updatedInstance else i')  <$> instances }
               )

    SkipInstance i -> do
      { instances } <- get
      let updatedInstance = i{instanceType = Skipped}
      createInstance updatedInstance >>=
        either (\err -> sendErrorNotification "There was a problem with the operation. Please try again.")
               (\_   -> 
                H.modify_ _{ instances = map (\i' -> if i == i' then updatedInstance else i')  <$> instances }
               )

    ApplyFilter f -> do
      H.modify_ _{ currentFilter = f}

    EndDateChanged s -> do
      newEndDate <- maybe defaultEndDate pure (de s)
      void $ H.fork $ handleAction $ LoadInstances newEndDate

    ScratchAmountChanged s -> do
      ScratchAmount c raw <- _.scratchAmount <$> get
      let modifiedScratch = ScratchAmount (fromMaybe (Currency 0.0) (Currency <$> fromString s)) s
      H.modify_ _{ scratchAmount = modifiedScratch }

defaultEndDate :: forall m. Now m => m EndDate
defaultEndDate = do
  today <- nowDate
  let adjusted = EndDate $ fromMaybe today $ adjust (Days 21.0) today
  pure adjusted


render :: forall m. State -> HH.ComponentHTML Action () m
render s = 
  HH.div [ HA.class_ Bootstrap.row ] 
  [ HH.div [ HA.class_ Bootstrap.col ] 
    [ renderInstances s ]
  , HH.div [ HA.class_ Bootstrap.col ] 
    [ scratchArea s ] 
  ]

renderInstances :: forall m. State -> H.ComponentHTML Action () m
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
                   , HE.onValueInput (Just <<< EndDateChanged)
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
                     , HE.onChecked (const <<< Just $ ApplyFilter filter)] 
          , HH.text text
          ] 
          where active = currentFilter == filter

ed :: EndDate -> String
ed = format conversionDateFormat <<< toDateTime <<< fromDate <<< unEndDate

de :: String -> Maybe EndDate
de = hush <<< ((<$>) (EndDate <<< date)) <<< unformat conversionDateFormat

prettyDate :: Date -> String
prettyDate = format dateFormat <<< toDateTime <<< fromDate

renderInstance :: forall m. Instance -> H.ComponentHTML Action () m
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

scratchArea :: forall r m. { scratchAmount :: ScratchAmount, instances :: RemoteData String (Array Instance) | r }  -> H.ComponentHTML Action () m
scratchArea { scratchAmount, instances } = 
  case scratchAmount, instances of
    ScratchAmount c s, Success i -> 
      HH.div_ 
      [ totalRow i
      , userAmountRow s   
      , remainingRow i c
      ]                      
    _, _         -> HH.div [] 
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
                                  , Bootstrap.colSm4
                                  , Bootstrap.textRight ]
                     , HA.value (formatCurrency $ total $ unactioned i)]]
    userAmountRow raw =
      row [ HH.label [HA.class_ Bootstrap.colSm3 ] 
              [ HH.text "In Account"]
          , HH.div [ HA.classes [ Bootstrap.inputGroup
                                , Bootstrap.mb2
                                , Bootstrap.mr2
                                , Bootstrap.p0
                                , Bootstrap.colSm4 ]]
            [ HH.div [ HA.class_ Bootstrap.inputGroupPrepend ] 
              [ HH.div [ HA.class_ Bootstrap.inputGroupText ]
                [ HH.text "$" ]]
            , HH.input [ HA.type_ InputText
                       , HA.value raw
                       , HA.classes [ Bootstrap.formControl, Bootstrap.textRight ]
                       , HE.onValueInput (Just <<< ScratchAmountChanged)]]]
    remainingRow i amt = 
      row [ HH.label [HA.class_ Bootstrap.colSm3 ] 
            [ HH.text "Left Over"] 
          , HH.input [ HA.readOnly true
                     , HA.classes [ Bootstrap.formControl
                                  , Bootstrap.colSm4
                                  , Bootstrap.textRight]
                     , HA.value (formatCurrency $ (total (unactioned i) - amt))]
          ]

      