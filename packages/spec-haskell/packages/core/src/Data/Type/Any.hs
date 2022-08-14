{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Data.Type.Any where

import           Data.Kind (Constraint, Type)


-- | ~AnyType~ ~a~ can hold wrap any instances of ~c~ constraint.
--
-- Note:
--    - It can be implemented with some flavor of existential type: https://wiki.haskell.org/Existential_type.
--
--    - ~a~ should also be an instance of ~c~. But it would require the ~UndecidableSuperClasses~ extension.
class c a => IsAnyTypeOf (a :: Type) (c :: Type -> Constraint) | a -> c where
    -- | Make a ~AnyType~ from a term that is of type that has ~c~ constraint.
    mkAny :: c e => e -> a
