module Budget.Api.Utils where
  
import Prelude

import Affjax (request)
import Budget.Api.Request (BaseURL, RequestOptions, defaultRequest)
import Budget.Capability.LogMessages (class LogMessages, logError)
import Budget.Capability.Now (class Now)
import Control.Monad.Reader (class MonadAsk, ask)
import Data.Argonaut (class DecodeJson, Json, decodeJson)
import Data.Argonaut.Decode ((.:))
import Data.Either (Either(..))
import Effect.Aff (attempt, message)
import Effect.Aff.Class (class MonadAff, liftAff)

mkRequest 
  :: forall m r
   . MonadAff m
  => MonadAsk { baseUrl :: BaseURL | r } m
  => RequestOptions
  -> m (Either String Json)
mkRequest opts = do
  { baseUrl } <- ask
  responseE <- liftAff <<< attempt $ request $ defaultRequest baseUrl opts
  case responseE of
    Left err -> pure $ Left $ message err
    Right response ->
      case response.body of
        Left _ -> pure $ (Left "Failed to decode response body.")
        Right json -> pure (Right json)
      

decodeAt :: forall a. DecodeJson a => String -> Json -> Either String a
decodeAt key = decodeJson <=< (_ .: key) <=< decodeJson

decode :: forall m a. LogMessages m => Now m => (Json -> Either String a) -> Either String Json -> m (Either String a)
decode _ (Left _) = logError "Response malformed" *> pure (Left "Response malformed")
decode decoder (Right json) = case decoder json of
  Left err -> logError err *> pure (Left err)
  Right response -> pure (Right response)
