{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex.Minter where

import           Data.Coerce                                                 (coerce)
import           Data.Default
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.Minter as MMUD
import           Money.Systems.Superfluid.Agreements.UniversalIndex.Data

-- * Monetary unit lenses
--

instance SuperfluidTypes sft => MMUD.MonetaryUnitLenses (UniversalIndex sft) sft where
    untappedValue = $(field 'minta_untapped_value)
    mintedValue   = $(field 'minta_minted_value)
type MinterMonetaryUnitData sft = MMUD.MonetaryUnitData (UniversalIndex sft) sft

-- * Contract
--

data MinterContractData sft = MinterContractData
instance Default (MinterContractData sft) where def = MinterContractData
instance SuperfluidTypes sft => AgreementContractData (MinterContractData sft) sft

-- * Operation
--

data MinterOperation sft = Mint (SFT_MVAL sft) |
                           Burn (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (MinterOperation sft)
         (MinterContractData sft) (MinterMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (MinterOperation sft) elem = MinterOperationPartiesF
        { mintFrom :: elem
        , mintTo   :: elem
        } deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Mint amount) acd _ = let
        acd'  = acd
        aopsΔ = fmap MMUD.MkMonetaryUnitData (MinterOperationPartiesF
                (def & set MMUD.mintedValue   (coerce (- amount)))
                (def & set MMUD.untappedValue (coerce    amount)))
        in (acd', aopsΔ)
    applyAgreementOperation (Burn amount) acd _ = let
        acd'  = acd
        aopsΔ = fmap MMUD.MkMonetaryUnitData (MinterOperationPartiesF
                (def & set MMUD.mintedValue   (coerce    amount))
                (def & set MMUD.untappedValue (coerce (- amount))))
        in (acd', aopsΔ)
