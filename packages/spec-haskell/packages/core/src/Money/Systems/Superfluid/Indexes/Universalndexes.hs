module Money.Systems.Superfluid.Indexes.Universalndexes where

import           Data.Default
import           Data.Kind                                                        (Type)

import           Money.Systems.Superfluid.Concepts                                (SuperfluidTypes (..))
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA

-- * ITA
--

type TBAMonetaryUnitData_ :: Type -> Type
data TBAMonetaryUnitData_ sft = TBAMonetaryUnitData ()
instance Default (TBAMonetaryUnitData_ sft) where def = TBAMonetaryUnitData ()
instance SuperfluidTypes sft => TBA.MonetaryUnitLenses (TBAMonetaryUnitData_ sft) sft
type TBAMonetaryUnitData sft = TBA.MonetaryUnitData (TBAMonetaryUnitData_ sft) sft

type TBAContractData_ :: Type -> Type
data TBAContractData_ sft = TBAContractData
instance Default (TBAContractData_ sft) where
    def = TBAContractData
instance SuperfluidTypes sft => TBA.ContractLenses (TBAContractData_ sft) sft
type TBAContractData sft = TBA.ContractData (TBAContractData_ sft) (TBAMonetaryUnitData_ sft) sft

-- * CFA
--

type CFAMonetaryUnitData_ :: Type -> Type
data CFAMonetaryUnitData_ sft = CFAMonetaryUnitData ()
instance Default (CFAMonetaryUnitData_ sft) where
    def = CFAMonetaryUnitData ()
instance SuperfluidTypes sft => CFA.MonetaryUnitLenses (CFAMonetaryUnitData_ sft) sft
type CFAMonetaryUnitData sft = CFA.MonetaryUnitData (CFAMonetaryUnitData_ sft) sft

type CFAContractData_ :: Type -> Type
data CFAContractData_ sft = CFAContractData ()
instance Default (CFAContractData_ sft) where def = CFAContractData ()
instance SuperfluidTypes sft => CFA.ContractLenses (CFAContractData_ sft) sft
type CFAContractData sft = CFA.ContractData (CFAContractData_ sft) (CFAMonetaryUnitData_ sft) sft


-- data CFAContractData sft = CFAMonetaryUnitData ()

-- * DFA
--

type DFAMonetaryUnitData_ :: Type -> Type
data DFAMonetaryUnitData_ sft = DFAMonetaryUnitData ()
instance Default (DFAMonetaryUnitData_ sft) where def = DFAMonetaryUnitData ()
instance SuperfluidTypes sft => DFA.MonetaryUnitLenses (DFAMonetaryUnitData_ sft) sft
type DFAMonetaryUnitData sft = DFA.MonetaryUnitData (DFAMonetaryUnitData_ sft) sft

type DFAContractData_ :: Type -> Type
data DFAContractData_ sft = DFAContractData ()
instance Default (DFAContractData_ sft) where def = DFAContractData ()
instance SuperfluidTypes sft => DFA.ContractLenses (DFAContractData_ sft) sft
type DFAContractData sft = DFA.ContractData (DFAContractData_ sft) (DFAMonetaryUnitData_ sft) sft
