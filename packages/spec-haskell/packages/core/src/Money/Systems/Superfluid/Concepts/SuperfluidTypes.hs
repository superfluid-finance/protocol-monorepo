{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.SuperfluidTypes
    ( Address
    , Timestamp
    , SFTFloat
    , SuperfluidTypes (..)
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Typeable                                     (Typeable)

import           Money.Concepts.Distribution                       (Liquidity)
import           Money.Systems.Superfluid.Concepts.RealtimeBalance (RealtimeBalance)


-- | Address Type Class
--
-- Naming conventions:
--  * Type name: addr
--  * Type family name: ACC_ADDR
class (Eq addr) => Address addr

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts) => Timestamp ts

-- | Actual Float Type
class (Default fr, RealFloat fr) => SFTFloat fr

-- | SuperfluidTypes Type Class
--
-- Naming conventions:
--  * Type name : sft
--
class ( Typeable sft
      , Typeable (SFT_LQ sft)
      , SFTFloat (SFT_FLOAT sft)
      , Liquidity (SFT_LQ sft)
      , Timestamp (SFT_TS sft)
      , RealtimeBalance (SFT_RTB sft) (SFT_LQ sft)
      , Address (SFT_ADDR sft)
      ) => SuperfluidTypes sft where

    -- Associated Type Synonyms
    type SFT_FLOAT sft :: Type
    type SFT_LQ sft :: Type
    type SFT_TS sft :: Type
    type SFT_RTB sft :: Type
    type SFT_ADDR sft :: Type
