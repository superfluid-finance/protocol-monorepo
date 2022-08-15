{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Type.Any where

import           Data.Kind  (Constraint, Type)
import           Data.Proxy (Proxy)


-- | ~AnyType~ ~a~ can hold wrap any instances of ~c~ constraint.
--
-- Note:
--    - It can be implemented with some flavor of existential type: https://wiki.haskell.org/Existential_type.
--
class IsAnyTypeOf (a :: Type) (c :: Type -> Constraint) | a -> c where
    -- | Make a ~AnyType~ from a term that is of type that has ~c~ constraint.
    mkAny :: c e => Proxy a -> e -> a

-- | Flip MPTC type params.
class c a b => MPTC_Flip c b a
instance c a b => MPTC_Flip c b a
