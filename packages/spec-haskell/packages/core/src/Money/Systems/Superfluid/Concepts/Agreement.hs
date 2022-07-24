{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementMonetaryUnitData (..)
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
      , Monoid amud
      ) => AgreementMonetaryUnitData amud sft | amud -> sft where
    -- | π function - balance provided (hear: π) by the agreement account data.
    balanceProvidedByAgreement
        :: amud          -- amud
        -> SFT_TS sft    -- t
        -> SFT_RTB sft   -- rtb

-- | Agreement operation type class.
--
-- Note: a. The constraint on ~amud~ may seems too strong, since a semigroup should also be sufficient. But why would you
--          define an operation that has nothing to do with any ~amud~, apart from the case of
--          ~NullAgreementMonetaryUnitData~?
--
--       b. It is conceivable that some ~amud~ is "read only" hence a "fake monoid", and their π is implicitly a
--          function of ~aod~. This class of ~amud~ is also known as "non-scalable", since ~amud~ is a product of ~aod~,
--          and a monetary unit would need as many ~amud~ as the needed ~aod~.
--
--       c. You may feel attempting to make ~amud~ an associated type family alias
--          e.g. ~AgreementOperationResultElem~. But don't waste your time, since ~sft~ is not injective. And making it
--          an associated data family seems rather cumbersome. I guess functional dependencies still has its syntactic
--          usefulness sometimes...
class ( SuperfluidTypes sft
      -- change to ~Semigroup amud~ can work too, see note (a).
      , AgreementMonetaryUnitData amud sft
      ) => AgreementOperation ao amud sft | ao -> sft, ao -> amud where
    -- | Areement operation data type.
    data AgreementOperationData ao :: Type

    -- | Agreement operation result container type.
    data AgreementOperationResultF ao elem :: Type

    -- | ω function - applying agreement operation ~ao~ (hear: ω) onto the agreement operation data ~aod~ to a result in
    --   functorful delta of agreement monetary unit data ~aorΔ~.
    applyAgreementOperation
        :: ao                                                             -- ao
        -> AgreementOperationData ao                                      -- aod
        -> SFT_TS sft                                                     -- t
        -> (AgreementOperationData ao, AgreementOperationResultF ao amud) -- (aod', aorΔ)

-- | A special null agreement monetary unit data.
--
-- Note: It is handy for agreement operation that does not modify any ~amud~, where their ~AgreementOperationResultF~ is
--       actually an empty container.
type NullAgreementMonetaryUnitData sft = ()
instance SuperfluidTypes sft => AgreementMonetaryUnitData (NullAgreementMonetaryUnitData sft) sft where
    balanceProvidedByAgreement _ _ = mempty
