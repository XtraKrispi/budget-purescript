module Budget.FFI.Toastr (ToastrOptions, ToastLevel(..), toast) where

import Prelude

import Data.Argonaut (class EncodeJson, fromString)
import Effect (Effect)

data ToastLevel = Info | Warning | Success | Error

toastLevelStr :: ToastLevel -> String
toastLevelStr Info = "info"
toastLevelStr Warning = "warning"
toastLevelStr Success = "success"
toastLevelStr Error = "error"

instance encodeToastLevel :: EncodeJson ToastLevel where
  encodeJson = fromString <<< toastLevelStr

type ToastrOptions = 
  { level :: ToastLevel
  , message :: String
  }

type ToastrOptionsRaw = 
  { level :: String
  , message :: String
  }

foreign import sendToast :: ToastrOptionsRaw -> Effect Unit

toast :: ToastrOptions -> Effect Unit
toast options = sendToast { level: toastLevelStr options.level, message: options.message}