{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.SuperfluidTypes
    ( Timestamp
    , SFTFloat
    , SuperfluidTypes (..)
    , SFT_RTB
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Proxy

import           Money.Concepts.Distribution                       (Value)
import           Money.Systems.Superfluid.Concepts.RealTimeBalance (RealTimeBalance)

-- | Superfluid float type.
class (Default fr, RealFloat fr) => SFTFloat fr

-- | Timestamp type class.
--
-- Notional conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts) => Timestamp ts

-- | Superfluid distribution type class.
--
-- Notional conventions:
--  * Type name : sft
class ( SFTFloat (SFT_FLOAT sft)
      , Value (SFT_MVAL sft)
      , Timestamp (SFT_TS sft)
      , RealTimeBalance (SFT_RTB_F sft) (SFT_MVAL sft)
      ) => SuperfluidTypes sft where
    type SFT_FLOAT sft :: Type
    type SFT_MVAL  sft :: Type
    type SFT_TS    sft :: Type
    type SFT_RTB_F sft :: Type -> Type

    dfa_default_lambda :: Proxy sft -> SFT_FLOAT sft

type SFT_RTB sft = SFT_RTB_F sft (SFT_MVAL sft)
