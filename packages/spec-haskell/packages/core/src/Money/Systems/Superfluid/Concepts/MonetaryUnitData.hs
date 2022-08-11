{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Concepts.MonetaryUnitData
    ( MonetaryUnitDataClass (..)
    , SemigroupMonetaryUnitData
    , AnySemigroupMonetaryUnitDataClass (..)
    , mud_prop_semigroup_settles_pi
    ) where

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Monetary unit data type class.
class ( SuperfluidTypes sft
      ) => MonetaryUnitDataClass mud sft | mud -> sft where
    -- | π function - balance provided (hear: π) by the monetary unit data.
    balanceProvided
        :: mud         -- mud
        -> SFT_TS sft  -- t
        -> SFT_RTB sft -- rtb

-- | A semigroup constrained monetary unit data type class.
--
-- Note: a. ~mud~ that doesn't have a binary function may also be referred to as "non-scalable" ~mud~.
--
--       a. What can make a ~mud~ "scalable" then is exactly when it is an actual semigroup. Since a new state can be
--          merged onto the previous state to a new single state. It is still worth mentioning that it is only a
--          sufficient condition, since a monoid could still "cheat" by linearly grow its data size on each binary
--          operation.
class ( MonetaryUnitDataClass mud sft
      , Semigroup mud
      ) => SemigroupMonetaryUnitData mud sft

-- | Existential type wrapper of semigroup monetary unit data
data AnySemigroupMonetaryUnitDataClass sft = forall mud. SemigroupMonetaryUnitData mud sft => MkMonetaryUnitDataClass mud

-- * Properties

-- | A semigroup binary operation should settle mud in a way that pi function output stay the same.
mud_prop_semigroup_settles_pi :: ( SuperfluidTypes sft
                                 , SemigroupMonetaryUnitData mud sft
                                 )
                              => mud -> mud -> SFT_TS sft -> Bool
mud_prop_semigroup_settles_pi m m' t = π m t <> π m' t == π (m <> m') t
    where π = balanceProvided
