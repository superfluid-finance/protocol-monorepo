{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementContract (..)
    ) where

import           Data.Default
import           Data.Kind                                          (Type)

import           Money.Systems.Superfluid.Concepts.MonetaryUnitData
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Agreement contract type class.
--
-- Note: TODO note about ~ac~ being a MUD.
class ( Default ac
      , Functor (AgreementOperationOutputF ac)
      , Traversable (AgreementOperationOutputF ac)
      ) => AgreementContract ac sft | ac -> sft where

    -- | ω function - apply agreement operation ~ao~ (hear: ω) onto the agreement operation data ~ac~ to get a tuple of:
    --
    --   1. An updated ~ac'~.
    --   2. A functorful delta of agreement monetary unit data ~aorΔ~, which then can be monoid-appended to existing ~mud~.
    --      This is what can make an agreement scalable.
    applyAgreementOperation
        :: ac                                   -- ac
        -> AgreementOperation ac                -- ao
        -> SFT_TS sft                           -- t
        -> (ac, AgreementOperationOutput ac)    -- (ac', mudsΔ) TODO rename to mudsΔ

    -- | φ function - functorize the existential monetary unit data of agreement parties
    functorizeAgreementOperationOutput
        :: forall f
         . f ~ AgreementOperationOutputF ac
        => AgreementOperationOutput ac
        -> f (AnyMonetaryUnitDataClass sft)

    -- Note: ~ac~ is injected, but it seems natural to have associated operation as data type instead.
    data AgreementOperation ac :: Type

    -- Note: ~ac~ is injected, hence this can be associated type alias.
    type AgreementOperationOutput ac :: Type

    -- Note: ~ac~ is not injected, hence this must be associated data type.
    data AgreementOperationOutputF ac :: Type -> Type
