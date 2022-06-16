{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Internal.TaggedTypeable where

import           Data.Typeable (Proxy (..), Typeable)

-- | Tagged Typeable
class (Typeable a, Typeable k) => TaggedTypeable (a :: k) where
    tagFromProxy :: Proxy a -> String
    tagFromType :: a -> String
    tagFromType _ = tagFromProxy (Proxy @a)
