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


-- | Address Type Class
--
-- Notional conventions:
--  * Type name: addr
--  * SuperfluidDistribution indexed type synonym: SFT_ADDR
class (Eq addr) => Address addr

-- | Timestamp Type Class
--
-- Notional conventions:
--  * Type name: ts
--  * SuperfluidDistribution type indexer: SFT_TS
class (Default ts, Integral ts) => Timestamp ts

-- | Actual Float Type
class (Default fr, RealFloat fr) => SFTFloat fr

-- | SuperfluidDistribution Type Class
--
-- Notional conventions:
--  * Type name : sfd
class ( Typeable sfd
      , SFTFloat (SFT_FLOAT sfd)
      , Liquidity (SFT_LQ sfd)
      , Timestamp (SFT_TS sfd)
      , RealtimeBalance (SFT_RTB sfd) (SFT_LQ sfd)
      , Address (SFT_ADDR sfd)
      ) => SuperfluidDistribution sfd where

    -- Associated Type Synonyms
    type SFT_FLOAT sfd :: Type
    type SFT_LQ sfd :: Type
    type SFT_TS sfd :: Type
    type SFT_RTB sfd :: Type
    type SFT_ADDR sfd :: Type
