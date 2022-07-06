{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    ) where

import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


-- | Agreement type class
class ( SuperfluidTypes sft
      , TaggedTypeable (AgreementContractData a)
      , TaggedTypeable (AgreementAccountData a), Semigroup (AgreementAccountData a)
      , Applicative (AgreementContractPartiesF a), Foldable (AgreementContractPartiesF a)
      ) => Agreement a sft | a -> sft where

    data AgreementContractData a :: Type

    data AgreementContractPartiesF a :: Type -> Type

    data AgreementAccountData a :: Type

    data AgreementOperation a :: Type

    -- | Balance provided by the agreement of an account
    balanceProvidedByAgreement :: AgreementAccountData a -> SFT_TS sft -> SFT_RTB sft

    -- | Create data of agreement parties from the changes of the agreement contract
    applyAgreementOperation :: AgreementContractData a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a sft => AgreementContractData a -> AgreementParties a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)
updateAgreement acd acps ao = let
    (acd', acpsDelta) = applyAgreementOperation acd ao
    -- applicatively map the semigroup binary operator over account data of agreement parites
    acps' = (<>) <$> acps <*> acpsDelta
    in (acd', acps')

-- ============================================================================
-- Internal Type Aliases
type AgreementParties a = (AgreementContractPartiesF a) (AgreementAccountData a)
