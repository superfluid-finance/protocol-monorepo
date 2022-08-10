{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementMonetaryUnitData (..)
    , amud_prop_semigroup_settles_pi
    , NullAgreementMonetaryUnitData
    , AgreementOperation (..)
    ) where

import           Data.Kind                                         (Type)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes

-- | Agreement monetary unit data type class.
--
-- Note: a. ~amud~ needs not to have a binary function, but when it does, it must conform to the monoid laws.
--
--       b. ~amud~ that doesn't have a binary function may also be referred to as "read only" ~amud~. See agreement
--          operation note below where it reveals a class of ~amud~ that is "non-scalable".
--
--       c. What can make a ~amud~ "scalable" then is exactly when it is a real monoid. Since a new state can be merged
--          onto the previous state to a new single state. It is still worth mentioning that it is only a sufficient
--          condition, since a monoid could still "cheat" by linearly grow its data size on each binary operation.
class ( SuperfluidTypes sft
      , Semigroup amud
      ) => AgreementMonetaryUnitData amud sft | amud -> sft where
    -- | π function - balance provided (hear: π) by the agreement monetary unit data.
    balanceProvidedByAgreement
        :: amud          -- amud
        -> SFT_TS sft    -- t
        -> SFT_RTB sft   -- rtb

amud_prop_semigroup_settles_pi :: ( SuperfluidTypes sft
                                  , AgreementMonetaryUnitData amud sft
                                  )
                               => amud -> amud -> SFT_TS sft -> Bool
amud_prop_semigroup_settles_pi m m' t = π m t <> π m' t == π (m <> m') t
    where π = balanceProvidedByAgreement

-- | Agreement operation type class.
--
-- It has three associated type/data families: ~aod~, ~aorF~ and ~amud~. See their documentations.
--
-- Note: a. The constraint on ~amud~ may seems too strong, since a semigroup should also be sufficient. But why would you
--          define an operation that has nothing to do with any ~amud~, apart from the trivial case of
--          ~NullAgreementMonetaryUnitData~?
--
--       b. It is conceivable that some ~amud~ are "read only" hence "fake monoid", where their π is implicitly a
--          function of ~aod~. This class of ~amud~ is also known as "non-scalable", since ~amud~ is a product of ~aod~,
--          and a monetary unit would need as many ~amud~ as the needed ~aod~.
class ( SuperfluidTypes sft
      -- change to ~Semigroup amud~ can work too, see note (a).
      , AgreementMonetaryUnitData (AgreementMonetaryUnitDataInOperation ao) sft
      ) => AgreementOperation ao sft | ao -> sft where
    -- | Areement operation data type ~aod~.
    data AgreementOperationData ao :: Type

    -- | Agreement operation result container type ~aorF~.
    data AgreementOperationResultF ao elem :: Type

    -- | Type of agreement monetary unit data ~amud~ created in operation result.
    type AgreementMonetaryUnitDataInOperation ao :: Type

    -- | ω function - apply agreement operation ~ao~ (hear: ω) onto the agreement operation data ~aod~ to get a tuple of:
    --
    --   1. An updated ~aod'~.
    --   2. A functorful delta of agreement monetary unit data ~aorΔ~, which then can be monoid-appended to existing ~amud~.
    --      This is what can make an agreement scalable.
    applyAgreementOperation
        :: amud ~ AgreementMonetaryUnitDataInOperation ao
        => ao                                   -- ao
        -> AgreementOperationData ao            -- aod
        -> SFT_TS sft                           -- t
        -> ( AgreementOperationData ao
           , AgreementOperationResultF ao amud) -- (aod', aorΔ)

-- | A special null agreement monetary unit data.
--
-- Note: It is handy for agreement operation that does not modify any ~amud~, where their ~AgreementOperationResultF~ is
--       actually an empty container.
type NullAgreementMonetaryUnitData sft = ()
instance SuperfluidTypes sft => AgreementMonetaryUnitData (NullAgreementMonetaryUnitData sft) sft where
    balanceProvidedByAgreement _ _ = mempty
