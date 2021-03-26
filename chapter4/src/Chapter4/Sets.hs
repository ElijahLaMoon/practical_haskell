module Chapter4.Sets where

import qualified Data.Map as M
import qualified Data.Set as S
import Chapter4.Client (Client(..))

data ClientKind = GovOrgKind | CompanyKind | IndividualKind deriving (Eq, Ord, Show)

classifyClients :: [Client] -> M.Map ClientKind (S.Set Client)
classifyClients = M.fromList . map auxClient
  where auxClient g@GovOrg {}     = (GovOrgKind, S.singleton g)
        auxClient c@Company {}    = (CompanyKind, S.singleton c)
        auxClient i@Individual {} = (IndividualKind, S.singleton i)