{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ImpredicativeTypes     #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementContract (..)
    , AgreementContractState
    , NullAgreementContract
    , AgreementOperation (..)
    , AgreementOperationOutputF (..)
    , AnyAgreementContractState (..)
    , ao_sum_contract_balance
    , ao_go_single_op
    , ao_go_zero_sum_balance_after_single_op
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

-- * Null Agreement

data NullAgreementContract sft

instance SuperfluidCoreTypes sft => Default (NullAgreementContract sft) where def = def
instance SuperfluidCoreTypes sft => MonetaryUnitDataClass (NullAgreementContract sft) sft

instance SuperfluidCoreTypes sft => AgreementContract (NullAgreementContract sft) sft where
    applyAgreementOperation ac _ _ = (ac, NullAgreementOutoutF)
    functorizeAgreementOperationOutput _ _ = NullAgreementOutoutF
    concatAgreementOperationOutput = const
    data AgreementOperation (NullAgreementContract sft) = NullAgreementOperation
    data AgreementOperationOutputF (NullAgreementContract sft) _ = NullAgreementOutoutF
        deriving stock (Functor, Foldable, Traversable)
    type AgreementOperationOutput (NullAgreementContract sft) = NullAgreementOutout sft

type NullAgreementOutout sft = AgreementOperationOutputF (NullAgreementContract sft) ()

instance SuperfluidCoreTypes sft => Default (NullAgreementOutout sft) where def = def

-- * Agreement Contract State

type AgreementContractState ac sft = (SuperfluidCoreTypes sft, AgreementContract ac sft)
    => (ac, AgreementOperationOutput ac)

data AnyAgreementContractState sft = forall ac. AgreementContract ac sft
    => MkAnyAgreementContractState (ac, AgreementOperationOutput ac)

-- =====================================================================================================================
-- * Agreement Laws

ao_sum_contract_balance :: forall sft any_smud.
                           ( SuperfluidCoreTypes sft
                           , MonetaryUnitDataClass any_smud sft
                           , any_smud `IsAnyTypeOf` MPTC_Flip SemigroupMonetaryUnitData sft
                           )
                        => Proxy any_smud
                        -> AnyAgreementContractState sft
                        -> SFT_TS sft
                        -> SFT_RTB sft
ao_sum_contract_balance p (MkAnyAgreementContractState (ac, muds)) t =
    foldr (<>) (π₂ t ac) (fmap (π₁ t) (φ muds))
    where φ  = functorizeAgreementOperationOutput p
          π₁ = flip balanceProvided -- π function (flipped) for semigroup mud
          π₂ = flip balanceProvided -- π function (flipped) for contract mud

ao_go_single_op :: forall ac sft.
                   ( SuperfluidCoreTypes sft
                   , AgreementContract ac sft
                   )
                => AgreementContractState ac sft
                -> AgreementOperation ac
                -> SFT_TS sft
                -> AgreementContractState ac sft
ao_go_single_op (ac, muds) ao t' =
    let (ac', mudsΔ) = ω ac ao t'
        muds'        = κ muds mudsΔ
    in  (ac', muds')
    where ω  = applyAgreementOperation
          κ  = concatAgreementOperationOutput

ao_go_zero_sum_balance_after_single_op :: forall ac sft any_smud.
                                          ( SuperfluidCoreTypes sft
                                          , AgreementContract ac sft
                                          , MonetaryUnitDataClass any_smud sft
                                          , any_smud
                                            `IsAnyTypeOf`
                                            MPTC_Flip SemigroupMonetaryUnitData sft
                                          )
                                       => Proxy any_smud
                                       -> (SFT_MVAL sft -> SFT_MVAL sft -> Bool)
                                       -> AgreementContractState ac sft
                                       -> AgreementOperation ac
                                       -> [AnyAgreementContractState sft]
                                       -> SFT_TS sft
                                       -> (Bool, AgreementContractState ac sft)
ao_go_zero_sum_balance_after_single_op p mvalEq cs ao ocss t' =
    let cs' = ao_go_single_op cs ao t'
    in  (σ cs `mvalEq` b && σ cs' `mvalEq` b, cs')
    where b = netValueOfRTB $ ao_sum_contract_balance p (MkAnyAgreementContractState cs) t'
          σ cs' = netValueOfRTB $
              foldMap (flip (ao_sum_contract_balance p) t') ocss <>
              ao_sum_contract_balance p (MkAnyAgreementContractState cs') t'
