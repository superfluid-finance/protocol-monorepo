{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    ) where

import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidDistribution (SuperfluidDistribution (..))


-- | Agreement type class
class ( SuperfluidDistribution (DistributionForAgreement a)
      , TaggedTypeable (AgreementContractData a)
      , TaggedTypeable (AgreementAccountData a), Semigroup (AgreementAccountData a)
      , Applicative (AgreementPartiesF a)
      ) => Agreement a where

    type DistributionForAgreement a :: Type

    data AgreementContractData a :: Type

    data AgreementPartiesF a :: Type -> Type

    data AgreementAccountData a :: Type

    data AgreementOperation a :: Type

    -- | Balance provided by the agreement of an account
    providedBalanceByAgreement :: AgreementAccountData a -> TS a -> RTB a

    -- | Create data of agreement parties from the changes of the agreement contract
    createAgreementPartiesDelta :: AgreementContractData a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a => AgreementContractData a -> AgreementParties a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)
updateAgreement acd aps ao = let (acd', apsDelta) = createAgreementPartiesDelta acd ao
    -- applicatively map the semigroup binary operator over parites and their
    in (acd', (<>) <$> aps <*> apsDelta)

-- ============================================================================
-- Internal Type Aliases
type TS a = SFT_TS (DistributionForAgreement a)
type RTB a = SFT_RTB (DistributionForAgreement a)
type AgreementParties a = (AgreementPartiesF a) (AgreementAccountData a)
