module Budget.Capability.LogMessages where
 
import Prelude

import Budget.Capability.Now (class Now)
import Budget.Data.Log (Log, LogReason(..), mkLog)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Halogen (HalogenM, lift)
 
class Monad m <= LogMessages m where
  logMessage :: Log -> m Unit

instance logMessagesHalogenM :: LogMessages m => LogMessages (HalogenM state action slots query m) where
  logMessage = lift <<< logMessage
  
log :: forall m. LogMessages m => Now m => LogReason -> String -> m Unit
log reason = logMessage <=< mkLog reason

logDebug :: forall m. LogMessages m => Now m => String -> m Unit
logDebug = log Debug

logInfo :: forall m. LogMessages m => Now m => String -> m Unit
logInfo = log Info

logWarn :: forall m. LogMessages m => Now m => String -> m Unit
logWarn = log Warn

logError :: forall m. LogMessages m => Now m => String -> m Unit
logError = log Error

logHush :: forall m a. LogMessages m => Now m => LogReason -> m (Either String a) -> m (Maybe a)
logHush reason action = 
  action >>= case _ of
    Left e -> case reason of
      Debug -> logDebug e *> pure Nothing 
      Info -> logInfo e *> pure Nothing 
      Warn -> logWarn e *> pure Nothing 
      Error -> logError e *> pure Nothing

    Right v -> pure $ Just v                  

debugHush :: forall m a. LogMessages m => Now m => m (Either String a) -> m (Maybe a)
debugHush = logHush Debug