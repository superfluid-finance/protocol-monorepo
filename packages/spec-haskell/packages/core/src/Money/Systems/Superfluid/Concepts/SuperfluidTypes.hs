{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.SuperfluidTypes
    ( Timestamp
    , SFTFloat
    , SuperfluidTypes (..)
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)

import           Money.Concepts.Distribution                       (Value)
import           Money.Systems.Superfluid.Concepts.RealtimeBalance (RealtimeBalance)


-- | Timestamp type class.
--
-- Notional conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts) => Timestamp ts

-- | Superfluid float type.
class (Default fr, RealFloat fr) => SFTFloat fr

-- | Superfluid distribution type class.
--
-- Notional conventions:
--  * Type name : sft
class ( SFTFloat (SFT_FLOAT sft)
      , Value (SFT_MVAL sft)
      , Timestamp (SFT_TS sft)
      , RealtimeBalance (SFT_RTB sft) (SFT_MVAL sft)
      ) => SuperfluidTypes sft where
    type SFT_FLOAT sft :: Type
    type SFT_MVAL  sft :: Type
    type SFT_TS    sft :: Type
    type SFT_RTB   sft :: Type
