{-# LANGUAGE TypeFamilyDependencies #-}

module Money.Systems.Superfluid.SystemTypes
    ( module Money.Systems.Superfluid.CoreTypes
    , module Money.Systems.Superfluid.Concepts.MonetaryUnitData
    , module Money.Systems.Superfluid.Concepts.Agreement
    , SuperfluidSystemTypes (..)
    ) where

import           Data.Kind                                          (Type)
import           Data.Proxy
import           Data.Type.Any

import           Money.Systems.Superfluid.CoreTypes
--
import           Money.Systems.Superfluid.Concepts.Agreement
import           Money.Systems.Superfluid.Concepts.MonetaryUnitData


-- | Superfuild system types as associated type family synonyms.
--
-- Note:
--
--   - This technique of indexing concrete type allows Implementer could put their own additional constraints that are
--   - particular to their system.
--
class ( SuperfluidCoreTypes sft
      , MonetaryUnitDataClass (SFT_ANY_MUD sft) sft
      -- TODO, use AnyType SFT_ANY_MUD sft `IsAnyTypeOf` MonetaryUnitDataClass sft
      ) => SuperfluidSystemTypes sft where

    -- | Default lambda value used by DFA.
    dfa_default_lambda :: Proxy sft -> SFT_FLOAT sft

    -- | Actual ~AnyMUD~ type.
    type family SFT_ANY_MUD sft = (any_mud :: Type) | any_mud -> sft
