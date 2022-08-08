module Money.Systems.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferValueTag
    , bufferValueTag
    , BufferValue
    , mkBufferValue
    ) where

import           Data.Proxy

import           Money.Systems.Superfluid.Concepts

-- TODO use TH: $(defineTappedValue BufferValueTag "b" BufferValue)
data BufferValueTag
instance TypedValueTag BufferValueTag where tappedValueTag _ = "b"
instance TappedValueTag BufferValueTag
type BufferValue v = TappedValue BufferValueTag v
bufferValueTag :: Proxy BufferValueTag
bufferValueTag = Proxy @BufferValueTag
mkBufferValue :: Value v => v -> BufferValue v
mkBufferValue = TappedValue
