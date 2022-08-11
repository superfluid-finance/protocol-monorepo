{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementOperation (..)
    ) where

import           Data.Kind                                          (Type)

import           Money.Systems.Superfluid.Concepts.MonetaryUnitData
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Agreement operation type class.
--
-- It has three associated type/data families: ~acd~, ~aorF~ and ~amud~. See their documentations.
--
-- Note: a. It is conceivable that some ~amud~ are "read only" hence "fake monoid", where their π is implicitly a
--          function of ~acd~. This class of ~amud~ is also known as "non-scalable", since ~amud~ is a product of ~acd~,
--          and a monetary unit would need as many ~amud~ as the needed ~acd~.
class ( SuperfluidTypes sft
      , MonetaryUnitDataClass (MonetaryUnitDataInOperation ao) sft
      ) => AgreementOperation ao sft | ao -> sft where
    -- | Areement operation data type ~acd~.
    data AgreementContract ao :: Type

    -- | Agreement operation result container type ~aorF~.
    data AgreementOperationResultF ao elem :: Type

    -- | Type of agreement monetary unit data ~amud~ created in operation result.
    type MonetaryUnitDataInOperation ao :: Type

    -- | ω function - apply agreement operation ~ao~ (hear: ω) onto the agreement operation data ~acd~ to get a tuple of:
    --
    --   1. An updated ~acd'~.
    --   2. A functorful delta of agreement monetary unit data ~aorΔ~, which then can be monoid-appended to existing ~amud~.
    --      This is what can make an agreement scalable.
    applyAgreementOperation
        :: amud ~ MonetaryUnitDataInOperation ao
        => ao                                   -- ao
        -> AgreementContract ao                 -- acd
        -> SFT_TS sft                           -- t
        -> ( AgreementContract ao
           , AgreementOperationResultF ao amud) -- (acd', aorΔ)
