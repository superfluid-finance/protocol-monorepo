{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Internal.TaggedTypeable where

import           Data.Typeable (Proxy (..), Typeable)

-- | Tagged typeable, giving a convenient string tag to a typeable
class (Typeable k, Typeable a) => TaggedTypeable (a :: k) where
    -- | Get the tag of a tagged typeable type through its proxy
    tagFromProxy :: Proxy a -> String

-- | Get the tag of a tagged typeable type through its value
tagFromValue :: forall a. (TaggedTypeable a) => a -> String
tagFromValue _ = tagFromProxy (Proxy @a)
