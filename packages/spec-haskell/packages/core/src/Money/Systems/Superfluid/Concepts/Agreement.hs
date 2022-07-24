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
class ( SuperfluidTypes sft
      , AgreementMonetaryUnitData amud sft
      ) => AgreementOperation ao amud sft | ao -> sft, ao -> amud where
    -- | Areement operation data type.
    data AgreementOperationData ao :: Type

    -- | Agreement operation result traversable applicative functor type.
    data AgreementOperationResultF ao elem :: Type

    -- | ω function - applying agreement operation ~ao~ (hear: ω) onto the agreement operation data ~aod~ to a result in
    --   functorful delta of agreement monetary unit data ~aorΔ~.
    applyAgreementOperation
        :: ao                                        -- ao
        -> AgreementOperationData ao                 -- aod
        -> SFT_TS sft                                -- t
        -> (AgreementOperationData ao
           , AgreementOperationResultF ao amud)      -- (aod', aorΔ)
