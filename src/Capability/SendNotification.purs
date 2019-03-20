module Budget.Capability.SendNotification (
   class SendNotification
  ,sendNotification
  ,Notification(..)
  ,NotificationLevel(..)
  ,mkInfo
  ,mkWarning
  ,mkError
  ,mkSuccess
  ,sendInfoNotification
  ,sendWarningNotification
  ,sendErrorNotification
  ,sendSuccessNotification) where
  
import Prelude

import Halogen (HalogenM, lift)
  
class Monad m <= SendNotification m where
  sendNotification :: Notification -> m Unit

data NotificationLevel = Info | Warning | Error | Success

data Notification = Notification String NotificationLevel

mkNotification :: forall a. Show a => NotificationLevel -> a -> Notification
mkNotification level msg = Notification (show msg) level

mkInfo :: forall a. Show a => a -> Notification
mkInfo = mkNotification Info <<< show

mkWarning :: forall a. Show a => a -> Notification
mkWarning = mkNotification Warning <<< show

mkError :: forall a. Show a => a -> Notification
mkError = mkNotification Error <<< show

mkSuccess :: forall a. Show a => a -> Notification
mkSuccess = mkNotification Success <<< show

sendInfoNotification :: forall a m. SendNotification m => Show a => a -> m Unit
sendInfoNotification = sendNotification <<< mkInfo

sendWarningNotification :: forall a m. SendNotification m => Show a => a -> m Unit
sendWarningNotification = sendNotification <<< mkWarning

sendErrorNotification :: forall a m. SendNotification m => Show a => a -> m Unit
sendErrorNotification = sendNotification <<< mkError

sendSuccessNotification :: forall a m. SendNotification m => Show a => a -> m Unit
sendSuccessNotification = sendNotification <<< mkSuccess

instance sendNotificationHalogenM :: SendNotification m => SendNotification (HalogenM s f g p o m) where
  sendNotification = lift <<< sendNotification
