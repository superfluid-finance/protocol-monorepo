{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Money.Superfluid.Concepts.SuperfluidTypes
    ( Address
    , SFTFloat
    , SuperfluidTypes (..)
    ) where

import           Data.Default                              (Default)
import           Data.Kind                                 (Type)
import           Data.Typeable                             (Typeable)

import           Money.Distribution.Concepts               (Timestamp)
import           Money.Superfluid.Concepts.Liquidity       (Liquidity)
import           Money.Superfluid.Concepts.RealtimeBalance


-- | Address Type Class
--
-- Naming conventions:
--  * Type name: addr
--  * Type family name: ACC_ADDR
class (Eq addr, Show addr) => Address addr

class (Default fr, RealFloat fr, Show fr) => SFTFloat fr

-- | SuperfluidTypes Type Class
--
-- Naming conventions:
--  * Type name : sft
--
class ( Typeable sft
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
