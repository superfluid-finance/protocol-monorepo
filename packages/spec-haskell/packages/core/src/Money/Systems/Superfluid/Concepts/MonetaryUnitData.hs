{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}

module Money.Systems.Superfluid.Concepts.MonetaryUnitData
    ( MonetaryUnitDataClass (..)
    , SemigroupMonetaryUnitData
    -- properties
    , mud_prop_semigroup_settles_pi
    ) where

import           Money.Systems.Superfluid.CoreTypes


-- | Monetary unit data type class.
class ( SuperfluidCoreTypes sft
      ) => MonetaryUnitDataClass mud sft | mud -> sft where
    -- | π function - balance provided (hear: π) by the monetary unit data.
    balanceProvided
        :: forall t rtb.
           -- indexed type aliases
           ( t ~ SFT_TS sft
           , rtb ~ SFT_RTB sft
           )
        => mud -> t -> rtb
    -- A default implementation that always returns zero rtb
    balanceProvided _ _ = mempty

-- | A semigroup constrained monetary unit data type class.
--
-- Note: a. ~mud~ that doesn't have a binary function may also be referred to as "non-scalable" ~mud~.
--
--       a. What can make a ~mud~ "scalable" then is exactly when it is an actual semigroup. Since a new state can be
--          merged onto the previous state to a new single state. It is still worth mentioning that it is only a
--          sufficient condition, since a monoid could still "cheat" by linearly grow its data size on each binary
--          operation.
class ( MonetaryUnitDataClass smud sft
      , Semigroup smud
      ) => SemigroupMonetaryUnitData smud sft

-- * Semigroup Monetary Unit Data Laws

-- | A semigroup binary operation should settle mud in a way that pi function output stay the same.
mud_prop_semigroup_settles_pi :: forall sft mud t.
                                 ( SuperfluidCoreTypes sft
                                 , SemigroupMonetaryUnitData mud sft
                                 -- indexed type aliases
                                 , t ~ SFT_TS sft
                                 )
                              => mud -> mud -> t -> Bool
mud_prop_semigroup_settles_pi m m' t = π m t <> π m' t == π (m <> m') t
    where π = balanceProvided
