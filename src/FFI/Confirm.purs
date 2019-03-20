module Budget.FFI.Confirm where

import Effect (Effect)

foreign import confirm :: String -> Effect Boolean