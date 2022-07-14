module Money.Systems.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferLiquidityTag
    , bufferLiquidityTag
    , BufferLiquidity
    , mkBufferLiquidity
    ) where

import           Data.Proxy

import           Money.Systems.Superfluid.Concepts

-- TODO use TH: $(defineTappedLiquidity BufferLiquidityTag "d" BufferLiquidity)
data BufferLiquidityTag
instance TappedValueTag BufferLiquidityTag where tappedValueTag _ = "b"
type BufferLiquidity v = TappedValue BufferLiquidityTag v
bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag
mkBufferLiquidity :: Value v => v -> BufferLiquidity v
mkBufferLiquidity = TappedValue
