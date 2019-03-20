module Budget.AppM where

import Prelude

import Budget.Api.Endpoint (Endpoint(..))
import Budget.Api.Request (BaseURL, RequestMethod(..))
import Budget.Api.Utils (decode, mkRequest)
import Budget.Capability.LogMessages (class LogMessages)
import Budget.Capability.Navigate (class Navigate)
import Budget.Capability.Now (class Now)
import Budget.Capability.Resource.Template (class ManageTemplate)
import Budget.Capability.SendNotification as N
import Budget.Data.Log as Log
import Budget.Data.Route as Route
import Budget.Data.Template (decodeTemplateWithKey, decodeTemplates, encodeTemplate)
import Control.Monad.Reader (class MonadAsk, ReaderT, ask, asks, runReaderT)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.String.Read (class Read)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Console as Console
import Effect.Now as Now
import Routing.Duplex (print)
import Routing.Hash (setHash)
import Budget.FFI.Toastr as T
import Type.Equality (class TypeEquals, from)

type Env = 
  { logLevel :: LogLevel
  , baseUrl  :: BaseURL
  }

data LogLevel = Dev | Prod
derive instance eqLogLevel :: Eq LogLevel
derive instance ordLogLevel :: Ord LogLevel

derive instance genericLogLevel :: Generic LogLevel _

instance showLogLevel :: Show LogLevel where
  show = genericShow

instance readLogLevel :: Read LogLevel where
  read "Dev" = Just Dev
  read "Prod" = Just Prod
  read _ = Just Dev

newtype AppM a = AppM (ReaderT Env Aff a)

runAppM :: Env -> AppM ~> Aff
runAppM env (AppM m) = runReaderT m env

derive newtype instance functorAppM :: Functor AppM
derive newtype instance applyAppM :: Apply AppM
derive newtype instance applicativeAppM :: Applicative AppM
derive newtype instance bindAppM :: Bind AppM
derive newtype instance monadAppM :: Monad AppM
derive newtype instance monadEffectAppM :: MonadEffect AppM
derive newtype instance monadAffAppM :: MonadAff AppM

instance monadAskAppM :: TypeEquals e Env => MonadAsk e AppM where
  ask = AppM $ asks from

instance nowAppM :: Now AppM where
  now = liftEffect Now.now
  nowDate = liftEffect Now.nowDate
  nowTime = liftEffect Now.nowTime
  nowDateTime = liftEffect Now.nowDateTime

instance logMessagesAppM :: LogMessages AppM where
  logMessage log = do
    env <- ask
    liftEffect case env.logLevel, Log.reason log of
      Prod, Log.Debug -> pure unit
      _, _ -> Console.log $ Log.message log

instance navigateAppM :: Navigate AppM where
  navigate = 
    liftEffect <<< setHash <<< print Route.routeCodec

instance manageTemplateAppM :: ManageTemplate AppM where
  getTemplate tId  = 
    mkRequest { endpoint: Template tId, method: Get }
      >>= decode decodeTemplateWithKey

  getTemplates     = 
    mkRequest { endpoint: Templates, method: Get } 
      >>= decode decodeTemplates

  createTemplate t = 
    let method = Post $ Just $ encodeTemplate t
    in  mkRequest { endpoint: Templates, method: method }
          >>= decode decodeTemplateWithKey

  updateTemplate tId t = 
    let method = Put $ Just $ encodeTemplate t
    in  ((<$>) (const unit)) <$> mkRequest { endpoint: Template tId, method: method }
         
  deleteTemplate tId = 
    ((<$>) (const unit)) <$> mkRequest { endpoint: Template tId, method: Delete }

instance sendNotificationAppM :: N.SendNotification AppM where
  sendNotification (N.Notification msg level) = 
    liftEffect $ T.toast { level: toastLevel level, message: msg }
    where toastLevel N.Info = T.Info
          toastLevel N.Warning = T.Warning
          toastLevel N.Error = T.Error
          toastLevel N.Success = T.Success