{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

-- | Instant transferring agreement.
--
-- This module is typically imported using qualified name MINTA.
module Money.Systems.Superfluid.Agreements.MinterAgreement where

import           Data.Coerce                                                 (coerce)
import           Data.Default
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import           Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.Minter as MMUD

-- * Monetary unit lenses
--

instance SuperfluidTypes sft => MMUD.MonetaryUnitLenses (UniversalData sft) sft where
    untappedValue = $(field 'minter_untapped_value)
    mintedValue   = $(field 'minter_minted_value)
type MonetaryUnitData sft = MMUD.MonetaryUnitData (UniversalData sft) sft

-- * Contract
--

data MinterContractData sft = MinterContractData
instance Default (MinterContractData sft) where def = MinterContractData

-- * Operation
--

data MinterOperation sft = Mint (SFT_MVAL sft) |
                           Burn (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (MinterOperation sft)
         (MinterContractData sft) (MonetaryUnitData sft) sft where
    data AgreementOperationResultF (MinterOperation sft) elem = MinterOperationPartiesF
        { mintFrom :: elem
        , mintTo   :: elem
        } deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Mint amount) acd _ = let
        acd'  = acd
        aorΔ = fmap MMUD.MkMonetaryUnitData (MinterOperationPartiesF
                (def & set MMUD.mintedValue   (coerce (- amount)))
                (def & set MMUD.untappedValue (coerce    amount)))
        in (acd', aorΔ)
    applyAgreementOperation (Burn amount) acd _ = let
        acd'  = acd
        aorΔ = fmap MMUD.MkMonetaryUnitData (MinterOperationPartiesF
                (def & set MMUD.mintedValue   (coerce    amount))
                (def & set MMUD.untappedValue (coerce (- amount))))
        in (acd', aorΔ)
