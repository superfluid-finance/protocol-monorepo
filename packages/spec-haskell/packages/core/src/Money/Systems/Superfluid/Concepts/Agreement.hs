{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementContract (..)
    , ao_prop_zero_sum_balance_series_ops
    ) where

import           Data.Default
import           Data.Kind                                          (Type)
import           Data.Proxy
import           Data.Type.Any

import           Money.Systems.Superfluid.Concepts.MonetaryUnitData
import           Money.Systems.Superfluid.CoreTypes


-- | Agreement contract type class.
class ( SuperfluidCoreTypes sft
      , Default ac
      , MonetaryUnitDataClass ac sft
      , Default (AgreementOperationOutput ac)
      , Traversable (AgreementOperationOutputF ac) -- <= Foldable Functor
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

    -- | φ' function - functorize the existential semigroup monetary unit data of agreement operation output
    functorizeAgreementOperationOutput
        :: forall muds f any_smud.
           ( muds ~ AgreementOperationOutput ac
           , f ~ AgreementOperationOutputF ac
           , MonetaryUnitDataClass any_smud sft
           , any_smud `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData sft
           )
        => Proxy any_smud -> muds -> f any_smud

    -- | κ function - concatenate agreement operation output, which can be functorized any time.
    concatAgreementOperationOutput
        :: forall muds.
           muds ~ AgreementOperationOutput ac
        => muds -> muds -> muds

    -- Note: though ~ac~ is injected, but it seems natural to have operation as associated data type instead.
    data family AgreementOperation ac :: Type

    -- Note: though ~ac~ is injected, none terminal type can only be associated data type.
    data family AgreementOperationOutputF ac :: Type -> Type

    -- Note: since ~ac~ is injected, hence this can be associated type alias.
    type family AgreementOperationOutput ac = (muds :: Type) | muds -> ac


-- =====================================================================================================================
-- * Agreement Laws

ao_is_zero_sum_balance :: forall ac sft any_smud.
                          ( SuperfluidCoreTypes sft
                          , AgreementContract ac sft
                          , MonetaryUnitDataClass any_smud sft
                          , any_smud `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData sft
                          )
                       => Proxy any_smud -> ac -> AgreementOperationOutput ac -> SFT_TS sft -> Bool
ao_is_zero_sum_balance p ac muds t = foldr (<>) (π₂ t ac) (fmap (π₁ t) (φ muds)) == mempty
    where φ  = functorizeAgreementOperationOutput p
          π₁ = flip balanceProvided -- π function (flipped) for semigroup mud
          π₂ = flip balanceProvided -- π function (flipped) for contract mud

ao_go_zero_sum_balance_single_op :: forall ac sft any_smud.
                                 ( SuperfluidCoreTypes sft
                                 , AgreementContract ac sft
                                 , MonetaryUnitDataClass any_smud sft
                                 , any_smud `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData sft
                                 )
                                 => Proxy any_smud
                                 -> ac
                                 -> AgreementOperationOutput ac
                                 -> AgreementOperation ac
                                 -> SFT_TS sft
                                 -> (Bool, ac, AgreementOperationOutput ac)
ao_go_zero_sum_balance_single_op p ac muds ao t' =
    let (ac', mudsΔ) = ω ac ao t'
        muds'        = κ muds mudsΔ
    in  ( ao_is_zero_sum_balance p ac  muds  t' &&
          ao_is_zero_sum_balance p ac' muds' t'
        , ac', muds')
    where ω  = applyAgreementOperation
          κ  = concatAgreementOperationOutput

-- | Series of agreement operations should result a funcorful of monetary unit data whose balance sum is always zero.
ao_prop_zero_sum_balance_series_ops :: forall ac sft any_smud.
                                       ( SuperfluidCoreTypes sft
                                       , AgreementContract ac sft
                                       , MonetaryUnitDataClass any_smud sft
                                       , any_smud `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData sft
                                       )
                                    => Proxy any_smud -> SFT_TS sft -> [(AgreementOperation ac, SFT_TS sft)] -> Bool
ao_prop_zero_sum_balance_series_ops p = go def def
    where go ac muds t ((ao, tΔ):aos) =
              let (good, ac', muds') = ao_go_zero_sum_balance_single_op p ac muds ao t'
              in  good && go ac' muds' t' aos
              where t' = t + tΔ
          go _ _ _ [] = True
