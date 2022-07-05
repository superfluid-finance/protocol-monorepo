{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    ) where

import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


-- | Agreement type class
class ( SuperfluidTypes (DistributionForAgreement a)
      , TaggedTypeable (AgreementContractData a)
      , TaggedTypeable (AgreementAccountData a), Semigroup (AgreementAccountData a)
      , Applicative (AgreementPartiesF a)
      ) => Agreement a where

    type DistributionForAgreement a :: Type

    data AgreementContractData a :: Type

    data AgreementPartiesF a :: Type -> Type

    data AgreementAccountData a :: Type

    -- | Balance provided by the agreement of an account
    providedBalanceByAgreement :: AgreementAccountData a -> TS a -> RTB a

    -- | Create data of agreement parties from the changes of the agreement contract
    createAgreementPartiesDelta :: AgreementContractData a -> AgreementContractData a -> AgreementParties a

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a => AgreementContractData a -> AgreementContractData a -> AgreementParties a -> AgreementParties a
updateAgreement old new parties = -- applicatively map the semigroup binary operator over parites and their delta
    (<>) <$> parties <*> partiesDelta
    where partiesDelta = createAgreementPartiesDelta old new

-- ============================================================================
-- Internal Type Aliases
type TS a = SFT_TS (DistributionForAgreement a)
type RTB a = SFT_RTB (DistributionForAgreement a)
type AgreementParties a = (AgreementPartiesF a) (AgreementAccountData a)
