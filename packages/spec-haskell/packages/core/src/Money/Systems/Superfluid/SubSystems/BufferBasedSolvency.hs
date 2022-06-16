{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications   #-}

module Money.Systems.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferLiquidityTag
    , bufferLiquidityTag
    , BufferLiquidity
    , mkBufferLiquidity
    ) where

-- import           Data.Default
import           Data.Typeable

import           Data.Internal.TaggedTypeable
import           Money.Systems.Superfluid.Concepts.Liquidity
    ( Liquidity
    , TappedLiquidity (..)
    , TappedLiquidityTag
    , TypedLiquidityTag
    )

-- TODO use TH: $(defineTappedLiquidity BufferLiquidityTag "d" BufferLiquidity)
data BufferLiquidityTag deriving anyclass (TypedLiquidityTag, TappedLiquidityTag)
instance TaggedTypeable BufferLiquidityTag where tagFromProxy _ = "b"
bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag
type BufferLiquidity lq = TappedLiquidity BufferLiquidityTag lq
mkBufferLiquidity :: Liquidity lq => lq -> BufferLiquidity lq
mkBufferLiquidity = TappedLiquidity
