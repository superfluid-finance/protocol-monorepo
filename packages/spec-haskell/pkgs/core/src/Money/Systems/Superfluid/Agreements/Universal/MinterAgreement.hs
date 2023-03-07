{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant transferring agreement.
--
-- This module is typically imported using qualified name MINTA.
module Money.Systems.Superfluid.Agreements.Universal.MinterAgreement where

import           Data.Coerce                                           (coerce)
import           Data.Default
import           Data.Type.Any
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue as MVMUD

-- * Monetary unit lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { untapped_value :: UntappedValue (SFT_MVAL sft)
    , minted_value   :: MVMUD.MintedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (MonetaryUnitLenses sft)

type MonetaryUnitData sft = MVMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft
instance SuperfluidSystemTypes sft => SemigroupMonetaryUnitData (MonetaryUnitData sft) sft

instance SuperfluidSystemTypes sft => MVMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    untappedValue = $(field 'untapped_value)
    mintedValue   = $(field 'minted_value)

-- * Contract & Operation
--

-- No ongoing relationships between parties
data ContractData sft = ContractData

instance SuperfluidSystemTypes sft => MonetaryUnitDataClass (ContractData sft) sft where

instance SuperfluidSystemTypes sft => Default (ContractData sft) where def = ContractData

instance SuperfluidSystemTypes sft => AgreementContract (ContractData sft) sft where
    applyAgreementOperation ac (Mint amount) _ = let
        ac' = ac
        mudsΔ = MVMUD.MkMonetaryUnitData <$> OperationOutputF
                (def & set MVMUD.mintedValue   (coerce (- amount)))
                (def & set MVMUD.untappedValue (coerce    amount))
        in (ac', mudsΔ)

    applyAgreementOperation ac (Burn amount) _ = let
        ac' = ac
        mudsΔ = fmap MVMUD.MkMonetaryUnitData (OperationOutputF
                (def & set MVMUD.mintedValue   (coerce    amount))
                (def & set MVMUD.untappedValue (coerce (- amount))))
        in (ac', mudsΔ)

    functorizeAgreementOperationOutput p = fmap (mkAny p)

    data AgreementOperation (ContractData sft) = Mint (SFT_MVAL sft) |
                                                 Burn (SFT_MVAL sft)

    type AgreementOperationOutput (ContractData sft) = OperationOutput sft

    data AgreementOperationOutputF (ContractData sft) elem = OperationOutputF
        { mint_from :: elem
        , mint_to   :: elem
        } deriving stock (Functor, Foldable, Traversable, Generic)

type OperationOutput sft = AgreementOperationOutputF (ContractData sft) (MonetaryUnitData sft)

instance SuperfluidSystemTypes sft => Default (OperationOutput sft)
instance SuperfluidSystemTypes sft => Monoid (OperationOutput sft) where mempty = def
instance SuperfluidSystemTypes sft => Semigroup (OperationOutput sft) where
    OperationOutputF a b <> OperationOutputF a' b' = OperationOutputF (a <> a') (b <> b')
