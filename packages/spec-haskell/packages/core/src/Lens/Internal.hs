{-# LANGUAGE TemplateHaskell #-}

module Lens.Internal
    ( module Lens.Micro
    , field
    ) where

import           Language.Haskell.TH
import           Lens.Micro


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
