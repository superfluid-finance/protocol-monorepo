{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementContract (..)
    , ao_prop_zero_sum_balance_series_ops
    ) where

import           Data.Default
import           Data.Kind                                          (Type)
import           Data.Proxy

import           Money.Systems.Superfluid.Concepts.MonetaryUnitData
import           Money.Systems.Superfluid.Concepts.SuperfluidTypes


-- | Agreement contract type class.
class ( Default ac
      , Default (AgreementOperationOutput ac)
      , Functor (AgreementOperationOutputF ac)
      , Foldable (AgreementOperationOutputF ac)
      , Traversable (AgreementOperationOutputF ac)
      , MonetaryUnitDataClass ac sft
      ) => AgreementContract ac sft | ac -> sft where

    -- | ω function - apply agreement operation ~ao~ (hear: ω) to the agreement operation data ~ac~ to get a tuple of:
    --
    --   1. An updated ~ac'~.
    --
    --   2. A functorful delta of agreement monetary unit data ~mudsΔ~, which then can be monoid-appended to existing
    --      ~mudΔ~.  This is what can make an agreement scalable.
    applyAgreementOperation
        :: ac                                -- ac
        -> AgreementOperation ac             -- ao
        -> SFT_TS sft                        -- t
        -> (ac, AgreementOperationOutput ac) -- (ac', mudsΔ)

    -- | κ' function -
    concatAgreementOperationOutput
        :: Proxy ac
        -> AgreementOperationOutput ac
        -> AgreementOperationOutput ac
        -> AgreementOperationOutput ac

    -- | φ' function - functorize the existential monetary unit data of agreement parties
    functorizeAgreementOperationOutput
        :: forall f.
           f ~ AgreementOperationOutputF ac
        => Proxy ac
        -> AgreementOperationOutput ac
        -> f (AnySemigroupMonetaryUnitData sft)

    -- Note: though ~ac~ is injected, but it seems natural to have operation as associated data type instead.
    data AgreementOperation ac :: Type

    -- Note: though ~ac~ is injected, none terminal type can only be associated data type.
    data AgreementOperationOutputF ac :: Type -> Type

    -- Note: since ~ac~ is injected, hence this can be associated type alias.
    type AgreementOperationOutput ac :: Type

-- * Agreement Laws

ao_go_zero_sum_balance_single_op :: forall ac sft.
                                 ( SuperfluidTypes sft
                                 , AgreementContract ac sft
                                 )
                                 => ac
                                 -> AgreementOperationOutput ac
                                 -> AgreementOperation ac
                                 -> SFT_TS sft
                                 -> (Bool, ac, AgreementOperationOutput ac)
ao_go_zero_sum_balance_single_op ac muds ao t' =
    let (ac', mudsΔ) = ω ac ao t'
        muds'        = κ muds mudsΔ
    in  ( foldr (<>) (π₂ t' ac ) (fmap (π₁ t') (φ muds )) == mempty &&
          foldr (<>) (π₂ t' ac') (fmap (π₁ t') (φ muds')) == mempty
        , ac', muds')
    where ω  = applyAgreementOperation
          κ  = concatAgreementOperationOutput (Proxy @ac)
          φ  = functorizeAgreementOperationOutput (Proxy @ac)
          π₁ = flip balanceProvided                           -- π function (flipped) for semigroup mud
          π₂ = flip (balanceProvided . MkAnyMonetaryUnitData) -- π function (flipped) for contract mud

-- | Series of agreement operations should result a funcorful of monetary unit data whose balance sum is always zero.
ao_prop_zero_sum_balance_series_ops :: forall ac sft.
                                       ( SuperfluidTypes sft
                                       , AgreementContract ac sft
                                       )
                                    => SFT_TS sft -> [(AgreementOperation ac, SFT_TS sft)] -> Bool
ao_prop_zero_sum_balance_series_ops = go def def
    where go ac muds t ((ao, tΔ):aos) =
              let (good, ac', muds') = ao_go_zero_sum_balance_single_op ac muds ao t'
              in good && go ac' muds' t' aos
              where t' = t + tΔ
          go _ _ _ [] = True
