module Budget.Capability.Resource.Instance where

import Prelude

import Budget.Data.Common (EndDate)
import Budget.Data.Instance (Instance)
import Budget.Data.Template (TemplateId)
import Data.Date (Date)
import Data.Either (Either)
import Halogen (HalogenM, lift)

class Monad m <= ManageInstance m where
  getInstances :: EndDate -> m (Either String (Array Instance))
  createInstance :: Instance -> m (Either String Unit)
  deleteInstance :: TemplateId -> Date -> m (Either String Unit)

instance manageInstanceHalogenM :: ManageInstance m => ManageInstance (HalogenM s f g p o m) where
  getInstances = lift <<< getInstances
  createInstance = lift <<< createInstance
  deleteInstance tId = lift <<< deleteInstance tId