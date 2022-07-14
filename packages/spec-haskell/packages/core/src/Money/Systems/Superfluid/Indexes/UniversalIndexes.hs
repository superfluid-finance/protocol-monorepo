{-# LANGUAGE TemplateHaskell #-}

module Money.Systems.Superfluid.Indexes.UniversalIndexes where

import           Data.Default
import           Data.Kind                                                        (Type)
import           Lens.Internal                                                    (field)

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS

-- * ITA
--

type TBAMonetaryUnitLens :: Type -> Type
data TBAMonetaryUnitLens sft = TBAMonetaryUnitData
    { tba_untappedLiquidity :: UntappedValue (SFT_LQ sft)
    , tba_mintedLiquidity   :: TBA.MintedLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => Default (TBAMonetaryUnitLens sft) where
    def = TBAMonetaryUnitData { tba_untappedLiquidity = def, tba_mintedLiquidity = def }
instance SuperfluidTypes sft => TBA.MonetaryUnitLens (TBAMonetaryUnitLens sft) sft where
    untappedLiquidity = $(field 'tba_untappedLiquidity)
    mintedLiquidity   = $(field 'tba_mintedLiquidity)
type TBAMonetaryUnitData sft = TBA.MonetaryUnitData (TBAMonetaryUnitLens sft) sft

type TBAContractLens :: Type -> Type
data TBAContractLens sft = TBAContractData
instance Default (TBAContractLens sft) where
    def = TBAContractData
instance SuperfluidTypes sft => TBA.ContractLens (TBAContractLens sft) sft
type TBAContractData sft = TBA.ContractData (TBAContractLens sft) (TBAMonetaryUnitLens sft) sft

-- * CFA
--

type CFAMonetaryUnitLens :: Type -> Type
data CFAMonetaryUnitLens sft = CFAMonetaryUnitData
    { cfa_settledAt                :: SFT_TS sft
    , cfa_settledUntappedLiquidity :: UntappedValue (SFT_LQ sft)
    , cfa_settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
    , cfa_netFlowRate              :: SFT_LQ sft
    }
instance SuperfluidTypes sft => Default (CFAMonetaryUnitLens sft) where
    def = CFAMonetaryUnitData { cfa_settledAt = def
                              , cfa_settledUntappedLiquidity = def
                              , cfa_settledBufferLiquidity = def
                              , cfa_netFlowRate = def
                              }
instance SuperfluidTypes sft => CFA.MonetaryUnitLens (CFAMonetaryUnitLens sft) sft where
    settledAt                = $(field 'cfa_settledAt)
    settledUntappedLiquidity = $(field 'cfa_settledUntappedLiquidity)
    settledBufferLiquidity   = $(field 'cfa_settledBufferLiquidity)
    netFlowRate              = $(field 'cfa_netFlowRate)
type CFAMonetaryUnitData sft = CFA.MonetaryUnitData (CFAMonetaryUnitLens sft) sft

type CFAContractLens :: Type -> Type
data CFAContractLens sft = CFAContractData
    { cfa_flowLastUpdatedAt :: SFT_TS sft
    , cfa_flowRate          :: SFT_LQ sft
    , cfa_flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => Default (CFAContractLens sft) where
    def = CFAContractData { cfa_flowLastUpdatedAt = def, cfa_flowRate = def, cfa_flowBuffer = def }
instance SuperfluidTypes sft => CFA.ContractLens (CFAContractLens sft) sft where
    flowLastUpdatedAt = $(field 'cfa_flowLastUpdatedAt)
    flowRate          = $(field 'cfa_flowRate)
    flowBuffer        = $(field 'cfa_flowBuffer)
type CFAContractData sft = CFA.ContractData (CFAContractLens sft) (CFAMonetaryUnitLens sft) sft

-- * DFA
--

type DFAMonetaryUnitLens :: Type -> Type
data DFAMonetaryUnitLens sft = DFAMonetaryUnitData
    { dfa_decayingFactor :: SFT_FLOAT sft
    , dfa_settledAt      :: SFT_TS sft
    , dfa_αVal           :: SFT_FLOAT sft
    , dfa_εVal           :: SFT_FLOAT sft
    , dfa_settledBuffer  :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => Default (DFAMonetaryUnitLens sft) where
    def = DFAMonetaryUnitData { dfa_decayingFactor = default_lambda
                              , dfa_settledAt = def
                              , dfa_αVal = def
                              , dfa_εVal = def
                              , dfa_settledBuffer = def
                              }
          where default_lambda = log 2 / (3600 * 24 * 7)
instance SuperfluidTypes sft => DFA.MonetaryUnitLens (DFAMonetaryUnitLens sft) sft where
    decayingFactor = $(field 'dfa_decayingFactor)
    settledAt      = $(field 'dfa_settledAt)
    αVal           = $(field 'dfa_αVal)
    εVal           = $(field 'dfa_εVal)
    settledBuffer  = $(field 'dfa_settledBuffer)
type DFAMonetaryUnitData sft = DFA.MonetaryUnitData (DFAMonetaryUnitLens sft) sft

type DFAContractLens :: Type -> Type
data DFAContractLens sft = DFAContractData
    { dfa_flowLastUpdatedAt :: SFT_TS sft
    , dfa_distributionLimit :: SFT_LQ sft
    , dfa_flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => Default (DFAContractLens sft) where
    def = DFAContractData { dfa_flowLastUpdatedAt = def, dfa_distributionLimit = def, dfa_flowBuffer = def }
instance SuperfluidTypes sft => DFA.ContractLens (DFAContractLens sft) sft where
    flowLastUpdatedAt = $(field 'dfa_flowLastUpdatedAt)
    distributionLimit = $(field 'dfa_distributionLimit)
    flowBuffer        = $(field 'dfa_flowBuffer)
type DFAContractData sft = DFA.ContractData (DFAContractLens sft) (DFAMonetaryUnitLens sft) sft
