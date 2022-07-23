{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementMonetaryUnitData (..)
    , AgreementOperation (..)
    ) where

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

-- | Agreement operation type class.
--
-- NOTE: Be aware of the functional dependency ~aod <-> ao~, this is important so that there is one ~aod~
--       for one ~ao~. We may as we index it using type family though (FIXME).
class ( SuperfluidTypes sft
      , AgreementMonetaryUnitData amud sft
      ) => AgreementOperation ao aod amud sft | ao -> sft, ao -> amud, ao -> aod, aod -> ao where
    -- | Agreement contract parties traversable applicative functor type.
    data AgreementOperationResultF ao elem :: Type

    -- | ω function - applying agreement operation (hear: ω) to a agreement contract data to get agreement account data
    --   delta of the agreement contract parties.
    applyAgreementOperation
        :: ao                                        -- ao
        -> aod                                       -- aod
        -> SFT_TS sft                                -- t
        -> (aod, AgreementOperationResultF ao amud) -- (aod', aorΔ)
