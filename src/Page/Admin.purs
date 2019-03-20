module Budget.Page.Admin where

import Prelude

import Budget.Capability.LogMessages (class LogMessages, logError)
import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now, nowDate)
import Budget.Capability.Resource.Template (class ManageTemplate, createTemplate, deleteTemplate, getTemplates, updateTemplate)
import Budget.Capability.SendNotification (class SendNotification)
import Budget.Capability.SendNotification as N
import Budget.Component.HTML.Utils (formatCurrency)
import Budget.Data.Common (Currency(..), StartDate(..), conversionDateFormat, unCurrency, unStartDate)
import Budget.Data.Template (Frequency(..), Template, TemplateWithKey)
import Budget.FFI.Confirm (confirm)
import Data.Array (sortBy, filter)
import Data.Date (Date, Month(..), canonicalDate)
import Data.DateTime (date)
import Data.DateTime.Instant (fromDate, toDateTime)
import Data.Either (either, hush)
import Data.Enum (toEnum)
import Data.Formatter.DateTime (FormatterCommand(..), format, unformat)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.List (List(..), head, (:))
import Data.Map as M
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Number (fromString)
import Data.Number.Format (toString)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff)
import Halogen (ClassName(..), get, liftEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties (InputType(..))
import Halogen.HTML.Properties as HA
import Halogen.Themes.Bootstrap4 as Boostrap
import Halogen.Themes.Bootstrap4 as Bootstrap
import Network.RemoteData (RemoteData(..), fromEither)
import Partial.Unsafe (unsafePartial)

type State = { templates    :: RemoteData String (Array TemplateWithKey)
             , sortSpec     :: SortSpec
             , editing      :: Maybe TemplateWithKey
             , newTemplate  :: Template ()
             }

data Query a = Initialize a
             | LoadTemplates a
             | Sort SortSpec a 
             | Edit TemplateWithKey a
             | ModifyDescription String a
             | ModifyAmount String a
             | CancelEditing a  
             | CommitEditing a
             | Delete TemplateWithKey a  
             | ModifyNewItemDescription String a  
             | ModifyNewItemAmount String a   
             | ModifyNewItemStartDate String a
             | ModifyNewItemFrequency String a     
             | AddNewItem a       

data SortDirection = Asc | Desc
derive instance eqSortDirection :: Eq SortDirection
derive instance genericSortDirection :: Generic SortDirection _
instance showSortDirection :: Show SortDirection where
  show = genericShow

data Column = DescriptionCol | AmountCol | StartDateCol | FrequencyCol

derive instance eqColumn :: Eq Column
derive instance genericColumn :: Generic Column _
instance showColumn :: Show Column where
  show = genericShow

data SortSpec = UnSorted | Sorted Column SortDirection

derive instance genericSortSpec :: Generic SortSpec _
instance showSortSpec :: Show SortSpec where
  show = genericShow

component
  :: forall m
   . MonadAff m
  => Navigate m
  => ManageTemplate m
  => LogMessages m
  => Now m
  => SendNotification m
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
  initialState _ = { templates: NotAsked
                   , sortSpec: UnSorted
                   , editing: Nothing
                   , newTemplate: initialNewTemplate (makeDate 1900 January 1)
                   }

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval = case _ of
    Initialize a -> do
      void $ H.fork $ eval $ LoadTemplates a
      { newTemplate } <- get
      today <- nowDate
      H.modify_ _{ newTemplate = newTemplate { startDate = StartDate today }}
      pure a

    LoadTemplates a -> do
      H.modify_ _{ templates = Loading }
      templates <- getTemplates
      H.modify_ _{ templates = fromEither templates }
      pure a
    
    Sort sorting a -> do
      H.modify_ _{ sortSpec = sorting }
      pure a

    Edit template a -> do
      H.modify_ _{ editing = Just template }
      pure a
    
    ModifyDescription d a -> do
      { editing } <- get
      case editing of
        Nothing -> pure a
        Just editing' -> do
          H.modify_ _{ editing = Just $ editing' { description = d } }
          pure a

    ModifyAmount amt a -> do
      { editing } <- get
      case editing, fromString amt of
        Just editing', Just amt' -> do
          H.modify_ _{ editing = Just $ editing' { amount = Currency amt' } }
          pure a
        _, _ -> pure a

    CancelEditing a -> do
      H.modify_ _{ editing = Nothing }
      pure a

    CommitEditing a -> do
      { editing, templates } <- get
      case editing, templates of
        Just editing', Success templates' -> do
          res <- updateTemplate editing'.templateId editing'
          either 
            (\err -> do
              N.sendErrorNotification "There was an issue saving the template, please try again."
              logError $ "Error saving template: " <> err
              pure a) 
            (\_ -> do
              let nt = (\t -> if t.templateId == editing'.templateId then editing' else t) <$> templates'
              H.modify_ _ { templates = Success nt, editing = Nothing }
              N.sendSuccessNotification "The template was updated successfully."
              pure a) res
        _, _ -> pure a
    Delete t a -> do
      toDelete <- liftEffect $ confirm "Are you sure you want to delete this template?"
      when toDelete $ do
        deleteTemplate t.templateId >>=      
        either 
            (\err -> do
              N.sendErrorNotification "There was an issue deleting the template, please try again."
              logError $ "Error deleting template: " <> err) 
            (\_ -> do
              { templates } <- get
              H.modify_ _{ templates = filter ((/=) t) <$> templates }
              N.sendSuccessNotification "The template was deleted successfully.")
      pure a
    
    ModifyNewItemDescription d a -> do
      { newTemplate } <- get
      H.modify_ _{ newTemplate = newTemplate { description = d }}
      pure a

    ModifyNewItemAmount amt a -> do
      { newTemplate } <- get
      maybe (pure unit) (\amt' -> H.modify_ _{ newTemplate = newTemplate { amount = Currency amt' }}) (fromString amt)
      pure a

    ModifyNewItemStartDate dt a -> do
      { newTemplate } <- get
      maybe (pure unit) (\v -> H.modify_ _{ newTemplate = newTemplate { startDate = v }}) $ ds dt
      pure a

    ModifyNewItemFrequency f a -> do
      { newTemplate } <- get
      maybe (pure unit) (\f' -> H.modify_ _{ newTemplate = newTemplate { frequency = f' }}) $ qref f
      pure a

    AddNewItem a -> do
      { newTemplate, templates } <- get
      case templates of
        Success ts -> do
          N.sendInfoNotification "Saving template..."
          createTemplate newTemplate >>=
          either 
            (\err -> do
              N.sendErrorNotification "There was an issue creating the template, please try again."
              logError $ "Error creating template: " <> err)
            (\t -> do
              today <- nowDate           
              H.modify_ _{ newTemplate = initialNewTemplate today, templates = Success $ ts <> [t]  }
              N.sendSuccessNotification "The template was created successfully.")
        _ -> pure unit
      pure a

render :: State -> H.ComponentHTML Query
render state = 
  let 
    pageContent = 
      case state.templates of
        NotAsked   -> HH.div [] [ HH.text "Admin" ]
        Loading    -> HH.div [] [ HH.text "Loading" ]
        Failure s  -> HH.div [] [ HH.text $ "Failed: " <> s ]
        Success ts -> 
          HH.div [] [
            HH.table [ HA.classes [ Bootstrap.table, Bootstrap.mt3 ] ] 
                     [ HH.thead_ 
                       [ HH.tr_ 
                         [ HH.th [ HA.class_ (ClassName "sort-col") ] [ HH.a [ changeSort DescriptionCol ] [ HH.text "Description" ], chevron DescriptionCol state.sortSpec ]
                         , HH.th [ HA.class_ (ClassName "sort-col") ] [ HH.a [ changeSort AmountCol ] [ HH.text "Amount" ] , chevron AmountCol state.sortSpec]
                         , HH.th [ HA.class_ (ClassName "sort-col") ] [ HH.a [ changeSort FrequencyCol ] [ HH.text "Frequency" ], chevron FrequencyCol state.sortSpec ]
                         , HH.th [ HA.class_ (ClassName "sort-col") ] [ HH.a [ changeSort StartDateCol ] [ HH.text "Start Date" ], chevron StartDateCol state.sortSpec ]
                         , HH.th_ [ HH.a [] [ HH.text "Actions" ] ]
                         ]                 
                       ]
                     , HH.tbody_ (((\t -> templateItem (isEditing state.editing t) t) <$> sort state.sortSpec ts) <> [newTemplateItem state.newTemplate])
                     ]         
          ]
  in pageContent
  where isEditing Nothing _ = false
        isEditing (Just e) t = e.templateId == t.templateId 
        changeSort col = 
          let sortSpec = case state.sortSpec of
                            Sorted col' dir -> if col == col' then
                                                Sorted col (reverseDir dir)
                                               else
                                                Sorted col Asc
                            _               -> Sorted col Asc
          in  HE.onClick (HE.input_ (Sort sortSpec))
        reverseDir Asc = Desc
        reverseDir Desc = Asc
        empty = HH.i [] []
        chevron _ UnSorted = empty
        chevron c (Sorted c' dir) 
          | c == c' = HH.i [HA.class_ $ ClassName $ "fas fa-angle-" <> (if dir == Asc then "up" else "down") ] []
          | otherwise = empty

sort :: SortSpec -> Array TemplateWithKey -> Array TemplateWithKey
sort UnSorted         = identity
sort (Sorted col dir) = sortBy order
  where order t1 t2 = 
          case col, dir of
            DescriptionCol, Asc  -> compare t1.description t2.description
            DescriptionCol, Desc -> compare t2.description t1.description
            AmountCol, Asc       -> compare t1.amount      t2.amount
            AmountCol, Desc      -> compare t2.amount      t1.amount
            FrequencyCol, Asc    -> compare t1.frequency   t2.frequency
            FrequencyCol, Desc   -> compare t2.frequency   t1.frequency
            StartDateCol, Asc    -> compare t1.startDate   t2.startDate  
            StartDateCol, Desc   -> compare t2.startDate   t1.startDate  

templateItem :: forall i. Boolean -> TemplateWithKey -> HH.HTML i (Query Unit)
templateItem false t = 
  HH.tr_  
  [ HH.td_ [ HH.text $ t.description ]
  , HH.td [ HA.class_ Bootstrap.textRight ] [ HH.text $ formatCurrency t.amount ]
  , HH.td_ [ HH.text $ freq t.frequency ]
  , HH.td_ [ HH.text $ prettySd t.startDate ]                         
  , HH.td_ [ HH.div [ HA.class_ (ClassName "actions") ] 
             [ action "far fa-edit" (Edit t)
             , action "far fa-trash-alt" (Delete t)
             ]
           ]
  ]
templateItem true t = 
  HH.tr_  
  [ HH.td_ [ HH.input [ HA.class_ Bootstrap.formControl
                      , HA.value t.description
                      , HE.onValueInput (HE.input ModifyDescription) ] ]
  , HH.td [ HA.class_ Bootstrap.textRight ] 
      [ HH.div [ HA.classes [ Bootstrap.inputGroup, Bootstrap.mb3 ]
               ]
        [ HH.div [ HA.class_ Bootstrap.inputGroupPrepend ] 
          [ HH.span [ HA.class_ Bootstrap.inputGroupText ] [ HH.text "$"]
          ]
        , HH.input [ HA.class_ Bootstrap.formControl
                   , HA.type_ InputNumber
                   , HA.value $ toString $ unCurrency t.amount 
                   , HE.onValueInput (HE.input ModifyAmount)
                   ] 
        ]
      ]
  , HH.td_ [ HH.text $ freq t.frequency ]
  , HH.td_ [ HH.text $ prettySd t.startDate ]                         
  , HH.td_ [ HH.div [ HA.class_ (ClassName "actions") ] 
             [ action "far fa-check-circle" CommitEditing
             , action "far fa-times-circle" CancelEditing
             ]
           ]
  ]

newTemplateItem :: forall i. Template () -> HH.HTML i (Query Unit)
newTemplateItem t = 
  HH.tr_ 
    [ HH. td_ [HH.input [ HA.class_ Bootstrap.formControl 
                        , HA.placeholder "description"
                        , HA.value t.description
                        , HE.onValueInput (HE.input ModifyNewItemDescription) ]                         
              ]
    , HH. td [ HA.class_ Bootstrap.textRight ] 
      [ HH.div [ HA.classes [ Bootstrap.inputGroup, Bootstrap.mb3 ]
               ]
        [ HH.div [ HA.class_ Bootstrap.inputGroupPrepend ] 
          [ HH.span [ HA.class_ Bootstrap.inputGroupText ] [ HH.text "$"]
          ]
        , HH.input [ HA.class_ Bootstrap.formControl
                   , HA.type_ InputNumber
                   , HA.value $ toString $ unCurrency t.amount 
                   , HE.onValueInput (HE.input ModifyNewItemAmount)
                   ] 
        ]
      ]
    , HH. td_ [ HH.select [ HA.class_ Bootstrap.formControl, HE.onValueChange (HE.input ModifyNewItemFrequency) ] $
                (\(Tuple k v) -> HH.option_ [HH.text v]) <$> M.toUnfoldable freqs
              ]
    , HH. td_ [ HH.input [ HA.class_ Bootstrap.formControl 
                         , HA.placeholder "start date"
                         , HA.type_ InputDate
                         , HA.value $ sd t.startDate
                         , HE.onValueInput (HE.input ModifyNewItemStartDate)
                         ] 
              ]
    , HH. td_ [ HH.button [HE.onClick (HE.input (const AddNewItem))
              , HA.classes [ Boostrap.btn, Bootstrap.btnPrimary ]
              , HA.disabled (t.description == "")]               
        [ HH.text "Add" ] ]
    ]  

freq :: Frequency -> String
freq f = fromMaybe "" $ M.lookup f freqs

qref :: String -> Maybe Frequency
qref str = head 
         $ map (\(Tuple k v) -> k) 
         $ M.toUnfoldable 
         $ M.filter ((==) str) freqs

freqs :: M.Map Frequency String
freqs = M.fromFoldable $ [ Tuple OneTime "One Time", Tuple BiWeekly "Bi-Weekly", Tuple Monthly "Monthly" ]

dateFormat :: List FormatterCommand
dateFormat = (MonthFull:Placeholder " ":DayOfMonth:Placeholder ", ":YearFull:Nil) 

prettySd :: StartDate -> String
prettySd = format dateFormat <<< toDateTime <<< fromDate <<< unStartDate

ds :: String -> Maybe StartDate
ds = hush <<< ((<$>) (StartDate <<< date)) <<< unformat conversionDateFormat

sd :: StartDate -> String
sd = format conversionDateFormat <<< toDateTime <<< fromDate <<< unStartDate

action :: forall i. String -> (Unit -> Query Unit) -> HH.HTML i (Query Unit)
action c a = HH.button [ HA.classes [ Bootstrap.btn, Bootstrap.btnSm, Bootstrap.bgTransparent ]
                     , HE.onClick (HE.input_ a)
                     ] 
                     [ HH.i [ HA.class_ (ClassName c) ] []
                     ]

initialNewTemplate :: Date -> Template ()
initialNewTemplate date = 
  { description: ""
  , amount: Currency 0.0
  , frequency: OneTime
  , startDate: StartDate date
  , isDeleted: false
  }

makeDate :: Int -> Month -> Int -> Date
makeDate year month day = 
    unsafePartial $ fromJust $ 
       canonicalDate <$> toEnum year <@> month <*> toEnum day  