module Budget.Capability.Resource.Instance where

import Prelude

import Budget.Data.Common (EndDate)
import Budget.Data.Instance (Instance)
import Data.Either (Either)
import Halogen (HalogenM, lift)

class Monad m <= ManageInstance m where
  getInstances :: EndDate -> m (Either String (Array Instance))

instance manageInstanceHalogenM :: ManageInstance m => ManageInstance (HalogenM s f g p o m) where
  getInstances = lift <<< getInstances