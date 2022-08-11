{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant transferring agreement.
--
-- This module is typically imported using qualified name MINTA.
module Money.Systems.Superfluid.Agreements.MinterAgreement where

import           Data.Coerce                                                      (coerce)
import           Data.Default
import           Data.Kind
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue as MVMUD

-- * Monetary unit lenses
--

data MonetaryUnitLenses sft = MonetaryUnitLenses
    { untapped_value :: UntappedValue (SFT_MVAL sft)
    , minted_value   :: MVMUD.MintedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MonetaryUnitLenses sft)

type MonetaryUnitData sft = MVMUD.MonetaryUnitData (MonetaryUnitLenses sft) sft

instance SuperfluidTypes sft => MVMUD.MonetaryUnitLenses (MonetaryUnitLenses sft) sft where
    untappedValue = $(field 'untapped_value)
    mintedValue   = $(field 'minted_value)

-- * Operation
--

data Operation sft = Mint (SFT_MVAL sft) |
                           Burn (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (Operation sft) sft where
    data AgreementContract (Operation sft) = ContractData
    data AgreementOperationResultF (Operation sft) elem = OperationResultF
        { mintFrom :: elem
        , mintTo   :: elem
        } deriving stock (Functor, Foldable, Traversable)
    type MonetaryUnitDataInOperation (Operation sft) = MonetaryUnitData sft

    applyAgreementOperation (Mint amount) acd _ = let
        acd'  = acd
        aorΔ = fmap MVMUD.MkMonetaryUnitData (OperationResultF
                (def & set MVMUD.mintedValue   (coerce (- amount)))
                (def & set MVMUD.untappedValue (coerce    amount)))
        in (acd', aorΔ)
    applyAgreementOperation (Burn amount) acd _ = let
        acd'  = acd
        aorΔ = fmap MVMUD.MkMonetaryUnitData (OperationResultF
                (def & set MVMUD.mintedValue   (coerce    amount))
                (def & set MVMUD.untappedValue (coerce (- amount))))
        in (acd', aorΔ)

type ContractData :: Type -> Type
type ContractData sft = AgreementContract (Operation sft)
instance SuperfluidTypes sft => Default (ContractData sft) where def = ContractData
