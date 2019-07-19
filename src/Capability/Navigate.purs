module Budget.Capability.Navigate where
  
import Prelude

import Budget.Data.Route (Route)
import Halogen (HalogenM, lift)

class Monad m <= Navigate m where
  navigate :: Route -> m Unit

instance navigateHalogenM :: Navigate m => Navigate (HalogenM state action slots query m) where
  navigate = lift <<< navigate