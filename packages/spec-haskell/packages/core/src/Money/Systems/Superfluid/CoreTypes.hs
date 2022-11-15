{-# LANGUAGE TypeFamilyDependencies #-}

module Money.Systems.Superfluid.CoreTypes
    ( module Money.Systems.Superfluid.CoreTypes.TypedValue
    , module Money.Systems.Superfluid.CoreTypes.RealTimeBalance
    , SFTFloat
    , RealTimeBalance (..)
    , SuperfluidCoreTypes (..)
    , SFT_RTB
    ) where

import           Data.Default                                       (Default)
import           Data.Kind                                          (Type)
import           Data.Typeable                                      (Typeable)

import           Money.Systems.Superfluid.CoreTypes.RealTimeBalance
import           Money.Systems.Superfluid.CoreTypes.TypedValue


-- | Superfluid float type. TODO naming?
class (Default fr, RealFloat fr) => SFTFloat fr

-- | Superfluid core types as associated type synonyms.
--
-- Notional conventions:
--  * Type name : sft
--
-- Note:
-- - Note the "6.4.9.7.1. Syntax of injectivity annotation"
class ( SFTFloat (SFT_FLOAT sft)
      , Typeable (SFT_MVAL sft), MonetaryValue (SFT_MVAL sft)
      , Timestamp (SFT_TS sft)
      , RealTimeBalance (SFT_RTB_F sft) (SFT_MVAL sft)
      ) => SuperfluidCoreTypes (sft :: Type) where
    type family SFT_FLOAT sft = (float :: Type)         | float -> sft
    type family SFT_MVAL  sft = (mval  :: Type)         | mval  -> sft
    type family SFT_TS    sft = (t     :: Type)         | t     -> sft
    type family SFT_RTB_F sft = (rtbF  :: Type -> Type) | rtbF  -> sft

type SFT_RTB sft = SFT_RTB_F sft (SFT_MVAL sft)
