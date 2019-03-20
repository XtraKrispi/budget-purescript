module Budget.Data.Log (LogReason(..), message, reason, Log, mkLog) where
  
import Prelude

import Affjax.RequestBody (RequestBody(..))
import Budget.Capability.Now (class Now, nowDateTime)
import Data.DateTime (DateTime)
import Data.Either (either)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Formatter.DateTime (formatDateTime)

data LogReason = Debug | Info | Warn | Error

derive instance genericLogReason :: Generic LogReason _
derive instance eqLogReason :: Eq LogReason
derive instance ordLogReason :: Ord LogReason

instance showLogReason :: Show LogReason where
  show = genericShow

newtype Log = Log
  { reason    :: LogReason
  , timestamp :: DateTime
  , message   :: String
  }

derive instance genericLog :: Generic Log _
derive instance eqLog :: Eq Log

message :: Log -> String
message (Log { message: m}) = m

reason :: Log -> LogReason
reason (Log { reason: r}) = r

timestamp :: Log -> DateTime
timestamp (Log { timestamp: t }) = t

mkLog :: forall m. Now m => LogReason -> String -> m Log
mkLog logReason inputMessage = do
  now <- nowDateTime
  let headerWith start = 
        "[" <> start <> ": " <> formatTimestamp now <> "]\n" <> inputMessage
      formattedLog = case logReason of
        Debug -> headerWith "DEBUG"
        Info  -> headerWith "INFO"
        Warn  -> headerWith "WARNING"
        Error  -> headerWith "ERROR"
  pure $ Log { reason: logReason, timestamp: now, message: formattedLog }

  where
    formatTimestamp =
      either (const "(Failed to assign time)") identity
        <<< formatDateTime "YYYY-DD-MM hh:mm:ss a"