{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.SuperfluidDistribution
    ( Address
    , Timestamp
    , SFTFloat
    , SuperfluidDistribution (..)
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Typeable                                     (Typeable)

import           Money.Concepts.Distribution                       (Liquidity)
import           Money.Systems.Superfluid.Concepts.RealtimeBalance (RealtimeBalance)


-- | Address type class.
--
-- Notional conventions:
--  * Type name: addr
--  * SuperfluidDistribution indexed type synonym: SFT_ADDR
class (Eq addr) => Address addr

-- | Timestamp type class.
--
-- Notional conventions:
--  * Type name: ts
--  * SuperfluidDistribution type indexer: SFT_TS
class (Default ts, Integral ts) => Timestamp ts

-- | Superfluid float type.
class (Default fr, RealFloat fr) => SFTFloat fr

-- | Superfluid distribution type class.
--
-- Notional conventions:
--  * Type name : sft
class ( Typeable sft
      , SFTFloat (SFT_FLOAT sft)
      , Liquidity (SFT_LQ sft)
      , Timestamp (SFT_TS sft)
      , RealtimeBalance (SFT_RTB sft) (SFT_LQ sft)
      , Address (SFT_ADDR sft)
      ) => SuperfluidDistribution sft where

    -- Associated Type Synonyms
    type SFT_FLOAT sft :: Type
    type SFT_LQ sft :: Type
    type SFT_TS sft :: Type
    type SFT_RTB sft :: Type
    type SFT_ADDR sft :: Type
