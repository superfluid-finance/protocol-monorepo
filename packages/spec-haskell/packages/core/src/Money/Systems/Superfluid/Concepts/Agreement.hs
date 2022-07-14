{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementMonetaryUnitData (..)
    , AgreementContractData (..)
    -- , updateAgreement
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


class ( SuperfluidTypes sft
      , TaggedTypeable amud
      , Monoid amud
      ) => AgreementMonetaryUnitData amud sft | amud -> sft where
    -- | Balance provided by the agreement account data
    balanceProvidedByAgreement
        :: amud          -- amud
        -> SFT_TS sft    -- t
        -> SFT_RTB sft   -- rtb

class ( SuperfluidTypes sft
      , AgreementMonetaryUnitData amud sft
      , TaggedTypeable acd
      , Default acd
      , Applicative (AgreementContractPartiesF acd), Traversable (AgreementContractPartiesF acd)
      ) => AgreementContractData acd amud sft | acd -> sft, acd -> amud where
    -- | Agreement contract parties traversable applicative functor type
    data AgreementContractPartiesF acd elem_t :: Type

    -- | Agreement operation algebraic data type
    data AgreementOperation acd :: Type

    -- | Applying agreement operation to a agreement contract data
    --   to get agreement account data delta of the agreement contract parties.
    applyAgreementOperation
        :: acd                                        -- acd
        -> AgreementContractPartiesF acd amud         -- acps
        -> AgreementOperation acd                     -- ao
        -> (acd, AgreementContractPartiesF acd amud)  -- (acd', acps')
