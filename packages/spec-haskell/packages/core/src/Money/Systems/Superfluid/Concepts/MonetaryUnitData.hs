{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.MonetaryUnitData
    ( MonetaryUnitDataClass (..)
    , amud_prop_semigroup_settles_pi
    , NullMonetaryUnitData
    ) where

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Monetary unit data type class.
--
-- Note: a. ~amud~ needs not to have a binary function, but when it does, it must conform to the semigroup laws.
--
--       b. ~amud~ that doesn't have a binary function may also be referred to as "read only" ~amud~.
--
--       c. What can make a ~amud~ "scalable" then is exactly when it is an actual semigroup. Since a new state can be
--          merged onto the previous state to a new single state. It is still worth mentioning that it is only a
--          sufficient condition, since a monoid could still "cheat" by linearly grow its data size on each binary
--          operation.
class ( SuperfluidTypes sft
      , Semigroup amud
      ) => MonetaryUnitDataClass amud sft | amud -> sft where
    -- | π function - balance provided (hear: π) by the monetary unit data.
    balanceProvided
        :: amud        -- amud
        -> SFT_TS sft  -- t
        -> SFT_RTB sft -- rtb

-- * AMUD properties

amud_prop_semigroup_settles_pi :: ( SuperfluidTypes sft
                                  , MonetaryUnitDataClass amud sft
                                  )
                               => amud -> amud -> SFT_TS sft -> Bool
amud_prop_semigroup_settles_pi m m' t = π m t <> π m' t == π (m <> m') t
    where π = balanceProvided

-- | A special null monetary unit data.
--
-- Note: It is handy for agreement operation that does not modify any ~amud~, where their ~AgreementOperationResultF~ is
--       actually an empty container.
type NullMonetaryUnitData sft = ()
instance SuperfluidTypes sft => MonetaryUnitDataClass (NullMonetaryUnitData sft) sft where
    balanceProvided _ _ = mempty
