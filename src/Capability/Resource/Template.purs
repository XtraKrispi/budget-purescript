module Budget.Capability.Resource.Template where
  
import Prelude

import Budget.Data.Template (Template, TemplateId, TemplateWithKey)
import Data.Either (Either)
import Halogen (HalogenM, lift)
  
class Monad m <= ManageTemplate m where
  getTemplate    :: TemplateId -> m (Either String TemplateWithKey) 
  getTemplates   :: m (Either String (Array TemplateWithKey))
  createTemplate :: forall t. Template t -> m (Either String TemplateWithKey)
  updateTemplate :: forall t. TemplateId -> Template t -> m (Either String Unit)
  deleteTemplate :: TemplateId -> m (Either String Unit)

instance manageTemplateHalogenM :: ManageTemplate m => ManageTemplate (HalogenM s f g p o m) where
  getTemplate      = lift <<< getTemplate
  getTemplates     = lift getTemplates
  createTemplate   = lift <<< createTemplate
  updateTemplate t = lift <<< updateTemplate t
  deleteTemplate   = lift <<< deleteTemplate