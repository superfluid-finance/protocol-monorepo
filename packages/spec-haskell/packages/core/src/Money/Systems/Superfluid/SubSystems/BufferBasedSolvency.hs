{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Money.Systems.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferLiquidityTag
    , bufferLiquidityTag
    , BufferLiquidity
    , mkBufferLiquidity
    ) where

import           Data.Type.TaggedTypeable
import           Data.Typeable

import           Money.Systems.Superfluid.Concepts.Liquidity (Liquidity, TappedLiquidity (..), TappedLiquidityTag)

-- TODO use TH: $(defineTappedLiquidity BufferLiquidityTag "d" BufferLiquidity)
data BufferLiquidityTag deriving anyclass (TappedLiquidityTag)
instance TaggedTypeable BufferLiquidityTag where tagFromProxy _ = "b"
type BufferLiquidity lq = TappedLiquidity BufferLiquidityTag lq
bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag
mkBufferLiquidity :: Liquidity lq => lq -> BufferLiquidity lq
mkBufferLiquidity = TappedLiquidity
