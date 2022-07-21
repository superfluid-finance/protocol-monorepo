{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementMonetaryUnitData (..)
    , AgreementContractData
    , AgreementOperation (..)
    ) where

import           Data.Default
import           Data.Kind                                         (Type)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Agreement monetary unit data type class.
class ( SuperfluidTypes sft
      , Monoid amud
      ) => AgreementMonetaryUnitData amud sft | amud -> sft where
    -- | π function - balance provided (hear: π) by the agreement account data.
    balanceProvidedByAgreement
        :: amud          -- amud
        -> SFT_TS sft    -- t
        -> SFT_RTB sft   -- rtb

-- | Agreement contract data type class.
--
-- Note the usage of functional dependency: the type of the monetary unit data is uniquely determined by the type of the
-- contract data.
-- class ( SuperfluidTypes sft
--       , AgreementMonetaryUnitData amud sft
--       , Default acd
--       , Applicative (AgreementContractPartiesF acd), Traversable (AgreementContractPartiesF acd)
--       ) => AgreementContractData acd amud sft | acd -> sft, acd -> amud where
--     -- | Agreement contract parties traversable applicative functor type.
--     data AgreementContractPartiesF acd elem :: Type

--     -- | Agreement operation algebraic data type.
--     data AgreementOperation acd :: Type

--     -- | ω function - applying agreement operation (hear: ω) to a agreement contract data to get agreement account data
--     --   delta of the agreement contract parties.
--     applyAgreementOperation
--         :: acd                                        -- acd
--         -> AgreementOperation acd                     -- ao
--         -> SFT_TS (sft)                               -- t
--         -> (acd, AgreementContractPartiesF acd amud)  -- (acd', acpΔamud)

-- | Agreement contract data type class.
class (SuperfluidTypes sft , Default acd) => AgreementContractData acd sft | acd -> sft

-- | Agreement operation type class.
class ( SuperfluidTypes sft
      , Traversable (AgreementOperationPartiesF ao)
      , AgreementContractData acd sft
      , AgreementMonetaryUnitData amud sft
      ) => AgreementOperation ao acd amud sft | ao -> sft, ao -> acd, ao -> amud where
    -- | Agreement contract parties traversable applicative functor type.
    data AgreementOperationPartiesF ao elem :: Type

    -- | ω function - applying agreement operation (hear: ω) to a agreement contract data to get agreement account data
    --   delta of the agreement contract parties.
    applyAgreementOperation
        :: ao                                        -- ao
        -> acd                                       -- acd
        -> SFT_TS sft                                -- t
        -> (acd, AgreementOperationPartiesF ao amud) -- (acd', aopsΔ)
