{-# LANGUAGE TypeFamilies #-}

module Data.Constraint.PreConstraintOf where

import           Data.Kind (Constraint, Type)

type family PreConstraintOf (a :: Type -> Constraint) :: Type -> Constraint

-- class (a b) => HasOneConstraint a b
-- class (a b, b c) => HasTwoConstraints a b c
-- class (b a, c a) => HasTwoConstraints a b c
-- type family OneConstraint a = HasOneConstraint b a
