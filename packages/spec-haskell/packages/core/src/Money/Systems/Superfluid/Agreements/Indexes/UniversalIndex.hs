{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex where

import           Data.Default
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement          as MINTA


-- | This is data that is universally available to the monetary unit.
--
-- NOTE: It is called universal index because of this reason: it is a trivial index
-- for the monetary unit to access the universal data.
data UniversalData sft = UniversalData -- UniversalMonetaryUnitData
    { _minta_lenses :: MINTA.MonetaryUnitLenses sft
    , _ita_lenses   :: ITA.MonetaryUnitLenses sft
    , _cfa_lenses   :: CFA.MonetaryUnitLenses sft
    , _dfa_lenses   :: DFA.MonetaryUnitLenses sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (UniversalData sft)

minta_lenses :: Lens' (UniversalData sft) (MINTA.MonetaryUnitLenses sft)
minta_lenses  = $(field '_minta_lenses)
ita_lenses   :: Lens' (UniversalData sft) (ITA.MonetaryUnitLenses sft)
ita_lenses    = $(field '_ita_lenses)
cfa_lenses   :: Lens' (UniversalData sft) (CFA.MonetaryUnitLenses sft)
cfa_lenses    = $(field '_cfa_lenses)
dfa_lenses   :: Lens' (UniversalData sft) (DFA.MonetaryUnitLenses sft)
dfa_lenses    = $(field '_dfa_lenses)
