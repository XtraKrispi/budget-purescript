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

mkNotification :: NotificationLevel -> String -> Notification
mkNotification level msg = Notification msg level

mkInfo :: String -> Notification
mkInfo = mkNotification Info

mkWarning :: String -> Notification
mkWarning = mkNotification Warning

mkError :: String -> Notification
mkError = mkNotification Error

mkSuccess :: String -> Notification
mkSuccess = mkNotification Success

sendInfoNotification :: forall m. SendNotification m => String -> m Unit
sendInfoNotification = sendNotification <<< mkInfo

sendWarningNotification :: forall m. SendNotification m => String -> m Unit
sendWarningNotification = sendNotification <<< mkWarning

sendErrorNotification :: forall m. SendNotification m => String -> m Unit
sendErrorNotification = sendNotification <<< mkError

sendSuccessNotification :: forall m. SendNotification m => String -> m Unit
sendSuccessNotification = sendNotification <<< mkSuccess

instance sendNotificationHalogenM :: SendNotification m => SendNotification (HalogenM s f g p o m) where
  sendNotification = lift <<< sendNotification
