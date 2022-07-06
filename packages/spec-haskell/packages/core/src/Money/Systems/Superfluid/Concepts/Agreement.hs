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
      , Applicative (AgreementPartiesF a), Foldable (AgreementPartiesF a)
      ) => Agreement a sft | a -> sft where

    data AgreementContractData a :: Type

    data AgreementPartiesF a :: Type -> Type

    data AgreementAccountData a :: Type

    data AgreementOperation a :: Type

    -- | Balance provided by the agreement of an account
    balanceProvidedByAgreement :: AgreementAccountData a -> SFT_TS sft -> SFT_RTB sft

    -- | Create data of agreement parties from the changes of the agreement contract
    applyAgreementOperation :: AgreementContractData a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a sft => AgreementContractData a -> AgreementParties a -> AgreementOperation a -> (AgreementContractData a, AgreementParties a)
updateAgreement acd aps ao = let
    (acd', apsDelta) = applyAgreementOperation acd ao
    -- applicatively map the semigroup binary operator over account data of agreement parites
    aps' = (<>) <$> aps <*> apsDelta
    in (acd', aps')

-- ============================================================================
-- Internal Type Aliases
type AgreementParties a = (AgreementPartiesF a) (AgreementAccountData a)
