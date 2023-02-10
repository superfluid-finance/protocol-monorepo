{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex where

import           Data.Default
import           GHC.Generics
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.Universal.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.Universal.InstantTransferAgreement as ITA
import qualified Money.Systems.Superfluid.Agreements.Universal.MinterAgreement          as MINTA


-- | This is data that is universally available to the monetary unit.
--
-- NOTE: It is called universal index because of this reason: it is a trivial index
-- for the monetary unit to access the universal data.
data UniversalData sft = UniversalData
    { _minta :: MINTA.MonetaryUnitLenses sft
    , _ita   :: ITA.MonetaryUnitLenses sft
    , _cfa   :: CFA.MonetaryUnitLenses sft
    , _dfa   :: DFA.MonetaryUnitLenses sft
    } deriving (Generic)
deriving instance SuperfluidSystemTypes sft => Default (UniversalData sft)

minta_lenses :: Lens' (UniversalData sft) (MINTA.MonetaryUnitLenses sft)
minta_lenses  = $(field '_minta)
ita_lenses   :: Lens' (UniversalData sft) (ITA.MonetaryUnitLenses sft)
ita_lenses    = $(field '_ita)
cfa_lenses   :: Lens' (UniversalData sft) (CFA.MonetaryUnitLenses sft)
cfa_lenses    = $(field '_cfa)
dfa_lenses   :: Lens' (UniversalData sft) (DFA.MonetaryUnitLenses sft)
dfa_lenses    = $(field '_dfa)
