{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Money.Superfluid.Concepts.TaggedTypeable where

import           Data.Typeable

class (Typeable a) => TaggedTypeable a where
    tagFromProxy :: Proxy a -> String
    tagFromType :: a -> String
    tagFromType _ = tagFromProxy (Proxy @a)
