{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.Concepts.SuperfluidDistribution
    ( LiquidityDistribution (..)
    , Address
    , Timestamp
    , SFTFloat
    , SuperfluidDistribution (..)
    ) where

import           Data.Default                                      (Default)
import           Data.Kind                                         (Type)
import           Data.Typeable                                     (Typeable)

import           Money.Concepts.Distribution                       (Liquidity, LiquidityDistribution (..))
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
--  * Type name : sfd
class ( Typeable sfd, LiquidityDistribution sfd
      , SFTFloat (SFT_FLOAT sfd)
      , Liquidity (SFT_LQ sfd)
      , Timestamp (SFT_TS sfd)
      , RealtimeBalance (SFT_RTB sfd) (SFT_LQ sfd)
      , Address (SFT_ADDR sfd)
      ) => SuperfluidDistribution sfd where
    type SFT_FLOAT sfd :: Type
    type SFT_LQ sfd :: Type
    type SFT_TS sfd :: Type
    type SFT_RTB sfd :: Type
    type SFT_ADDR sfd :: Type
