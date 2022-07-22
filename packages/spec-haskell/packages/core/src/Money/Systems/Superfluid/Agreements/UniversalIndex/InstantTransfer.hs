{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex.InstantTransfer where

import           Data.Coerce
import           Data.Default
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer as ITMUD
import           Money.Systems.Superfluid.Agreements.UniversalIndex.Data

-- * Monetary data lenses
--

instance SuperfluidTypes sft => ITMUD.MonetaryUnitLenses (UniversalData sft) sft where
    untappedValue = $(field 'ita_untapped_value)
type ITAMonetaryUnitData sft = ITMUD.MonetaryUnitData (UniversalData sft) sft

-- * Contract
--

data ITAContractData sft = ContractData
instance Default (ITAContractData sft) where def = ContractData

-- * Operation
--

data ITAOperation sft = Transfer (SFT_MVAL sft)

instance SuperfluidTypes sft => AgreementOperation (ITAOperation sft)
    (ITAContractData sft) (ITAMonetaryUnitData sft) sft where
    data AgreementOperationPartiesF (ITAOperation sft) elem = ITAOperationPartiesF
        { transferFrom :: elem
        , transferTo   :: elem
        } deriving stock (Functor, Foldable, Traversable)

    applyAgreementOperation (Transfer amount) acd _ = let
        acd'  = acd
        aopsΔ = fmap ITMUD.MkMonetaryUnitData (ITAOperationPartiesF
                    (def & set ITMUD.untappedValue (coerce (- amount)))
                    (def & set ITMUD.untappedValue (coerce    amount)))
        in (acd', aopsΔ)
