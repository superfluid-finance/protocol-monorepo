{-# LANGUAGE TemplateHaskell #-}

module Lens.Internal
    ( module Lens.Micro
    , module Lens.Micro.Extras
    , readOnlyLens
    , field
    ) where

import           Language.Haskell.TH
import           Lens.Micro
import           Lens.Micro.Extras

-- | A short hand for creating a read only lens
readOnlyLens :: (s -> a) -> Lens s t a b
readOnlyLens g = lens g (error "setting a read only lens")

-- | Make a lens from a field name.
--
-- Example: @over $(field 'foo) (*2)@
-- NOTE: Copied from basic-lens
field :: Name -> Q Exp
field name = do
  [|\f r ->
      fmap
        $(lamE
            [varP (mkName "a")]
            (recUpdE (varE (mkName "r")) [return (name, VarE (mkName "a"))]))
        (f ($(varE name) r))|]
