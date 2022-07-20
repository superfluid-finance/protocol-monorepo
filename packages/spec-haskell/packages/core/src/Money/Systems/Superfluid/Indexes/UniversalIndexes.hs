{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Money.Systems.Superfluid.Indexes.UniversalIndexes where

import           Data.Default
import           Data.Proxy
import           GHC.Generics

import           Lens.Internal                                                (field, lens)

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement          as MINTA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency      as BBS

-- * MINTA
--

data MINTAMonetaryUnitLens sft = MINTAMonetaryUnitData
    { minta_untapped_value :: UntappedValue (SFT_MVAL sft)
    , minta_minted_value   :: MINTA.MintedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (MINTAMonetaryUnitLens sft)

instance SuperfluidTypes sft => MINTA.MonetaryUnitLens (MINTAMonetaryUnitLens sft) sft where
    untappedValue = $(field 'minta_untapped_value)
    mintedValue   = $(field 'minta_minted_value)
type MINTAMonetaryUnitData sft = MINTA.MonetaryUnitData (MINTAMonetaryUnitLens sft) sft

-- not much to look into really
data MINTAContractLens sft = MINTAContractData deriving (Generic, Default)

instance SuperfluidTypes sft => MINTA.ContractLens (MINTAContractLens sft) sft
type MINTAContractData sft = MINTA.ContractData (MINTAContractLens sft) (MINTAMonetaryUnitLens sft) sft

-- * ITA
--

data ITAMonetaryUnitLens sft = ITAMonetaryUnitData
    { ita_untapped_value :: UntappedValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (ITAMonetaryUnitLens sft)

instance SuperfluidTypes sft => ITA.MonetaryUnitLens (ITAMonetaryUnitLens sft) sft where
    untappedValue = $(field 'ita_untapped_value)
type ITAMonetaryUnitData sft = ITA.MonetaryUnitData (ITAMonetaryUnitLens sft) sft

-- not much to look into really
data ITAContractLens sft = ITAContractData deriving (Generic, Default)

instance SuperfluidTypes sft => ITA.ContractLens (ITAContractLens sft) sft
type ITAContractData sft = ITA.ContractData (ITAContractLens sft) (ITAMonetaryUnitLens sft) sft

-- * CFA
--

data CFAMonetaryUnitLens sft = CFAMonetaryUnitData
    { cfa_settled_at             :: SFT_TS sft
    , cfa_settled_untapped_value :: UntappedValue (SFT_MVAL sft)
    , cfa_settled_buffer_value   :: BBS.BufferValue (SFT_MVAL sft)
    , cfa_net_flow_rate          :: SFT_MVAL sft
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (CFAMonetaryUnitLens sft)

instance SuperfluidTypes sft => CFA.MonetaryUnitLens (CFAMonetaryUnitLens sft) sft where
    settledAt            = $(field 'cfa_settled_at)
    settledUntappedValue = $(field 'cfa_settled_untapped_value)
    settledBufferValue   = $(field 'cfa_settled_buffer_value)
    netFlowRate          = $(field 'cfa_net_flow_rate)
type CFAMonetaryUnitData sft = CFA.MonetaryUnitData (CFAMonetaryUnitLens sft) sft

data CFAContractLens sft = CFAContractData
    { cfa_flow_last_updated_at :: SFT_TS sft
    , cfa_flow_rate            :: SFT_MVAL sft
    , cfa_flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (CFAContractLens sft)

instance SuperfluidTypes sft => CFA.ContractLens (CFAContractLens sft) sft where
    flowLastUpdatedAt = $(field 'cfa_flow_last_updated_at)
    flowRate          = $(field 'cfa_flow_rate)
    flowBuffer        = $(field 'cfa_flow_buffer)
type CFAContractData sft = CFA.ContractData (CFAContractLens sft) (CFAMonetaryUnitLens sft) sft

-- * DFA
--

-- type DFAMonetaryUnitLens :: Type -> Type
data DFAMonetaryUnitLens sft = DFAMonetaryUnitData
    { dfa_settledAt     :: SFT_TS sft
    , dfa_αVal          :: SFT_FLOAT sft
    , dfa_εVal          :: SFT_FLOAT sft
    , dfa_settledBuffer :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DFAMonetaryUnitLens sft)

instance SuperfluidTypes sft => DFA.MonetaryUnitLens (DFAMonetaryUnitLens sft) sft where
    decayingFactor = lens (\_ -> dfa_default_lambda (Proxy @sft)) (error "UIDX dfa λ cannot be set")
    settledAt      = $(field 'dfa_settledAt)
    αVal           = $(field 'dfa_αVal)
    εVal           = $(field 'dfa_εVal)
    settledBuffer  = $(field 'dfa_settledBuffer)
type DFAMonetaryUnitData sft = DFA.MonetaryUnitData (DFAMonetaryUnitLens sft) sft

data DFAContractLens sft = DFAContractData
    { dfa_flow_last_updated_at :: SFT_TS sft
    , dfa_distribution_limit   :: SFT_MVAL sft
    , dfa_flow_buffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (DFAContractLens sft)

instance SuperfluidTypes sft => DFA.ContractLens (DFAContractLens sft) sft where
    flowLastUpdatedAt = $(field 'dfa_flow_last_updated_at)
    distributionLimit = $(field 'dfa_distribution_limit)
    flowBuffer        = $(field 'dfa_flow_buffer)
type DFAContractData sft = DFA.ContractData (DFAContractLens sft) (DFAMonetaryUnitLens sft) sft
