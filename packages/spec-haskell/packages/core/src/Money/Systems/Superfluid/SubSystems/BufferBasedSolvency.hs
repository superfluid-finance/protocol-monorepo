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

import           Money.Systems.Superfluid.Concepts.TypedValue (Value, TappedValue (..), TappedValueTag)

-- TODO use TH: $(defineTappedLiquidity BufferLiquidityTag "d" BufferLiquidity)
data BufferLiquidityTag deriving anyclass (TappedValueTag)
instance TaggedTypeable BufferLiquidityTag where tagFromProxy _ = "b"
type BufferLiquidity v = TappedValue BufferLiquidityTag v
bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag
mkBufferLiquidity :: Value v => v -> BufferLiquidity v
mkBufferLiquidity = TappedValue
