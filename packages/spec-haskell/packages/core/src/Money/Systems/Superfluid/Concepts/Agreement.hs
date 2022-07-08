{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


-- | Agreement type class
class ( SuperfluidTypes sft
      , TaggedTypeable (AgreementContractData a), Default (AgreementContractData a)
      , Applicative (AgreementContractPartiesF a), Traversable (AgreementContractPartiesF a)
      , TaggedTypeable (AgreementAccountData a), Semigroup (AgreementAccountData a)
      ) => Agreement a sft | a -> sft where
    -- | Agreement contract data type
    data AgreementContractData a :: Type

    -- | Agreement contract parties traversable applicative functor type
    data AgreementContractPartiesF a :: Type -> Type

    -- | Agreement account semigroup data type
    data AgreementAccountData a :: Type

    -- | Agreement operation algebraic data type
    data AgreementOperation a :: Type

    -- | Balance provided by the agreement account data
    balanceProvidedByAgreement
        :: AgreementAccountData a -- aad
        -> SFT_TS sft             -- t
        -> SFT_RTB sft            -- returns rtb

    -- | Applying agreement operation to a agreement contract data
    --   to get agreement account data delta of the agreement contract parties.
    applyAgreementOperation
        :: AgreementContractData a                               -- acd
        -> AgreementOperation a                                  -- ao
        -> (AgreementContractData a, AgreementContractParties a) -- (acd', acpsDelta)

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: (SuperfluidTypes sft, Agreement a sft)
    => AgreementContractData a
    -> AgreementContractParties a
    -> AgreementOperation a
    -> (AgreementContractData a, AgreementContractParties a)
updateAgreement acd acps ao = let
    (acd', acpsDelta) = applyAgreementOperation acd ao
    -- Applicatively fmap (<$> <*>) the semigroup binary operator (<>)
    -- over the current value (acps) and the delta (acpsDelta)
    -- of the agreement account data of the agreement contract parties
    acps' = (<>) <$> acps <*> acpsDelta
    in (acd', acps')

-- ============================================================================
-- * Internal Type Aliases

type AgreementContractParties a = (AgreementContractPartiesF a) (AgreementAccountData a)
