module Budget.FFI.Config (environment) where

import Prelude

import Budget.Api.Request (BaseURL(..))
import Budget.AppM (Env, LogLevel(..))
import Data.Maybe as Maybe
import Data.String.Read (read)
import Foreign (F, Foreign, readString)
import Foreign.Index ((!))

foreign import env :: Foreign

environment :: F Env
environment = do
  let value = env
  rawBaseUrl <- value ! "baseUrl"
  rawLogLevel <- value ! "logLevel"
  baseUrl <- BaseURL <$> readString rawBaseUrl
  logLevel <- read <$> readString rawLogLevel
  pure { baseUrl: baseUrl,logLevel:  Maybe.fromMaybe Dev logLevel }