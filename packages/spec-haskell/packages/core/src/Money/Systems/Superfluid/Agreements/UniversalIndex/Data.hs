{-# LANGUAGE DeriveAnyClass #-}

module Money.Systems.Superfluid.Agreements.UniversalIndex.Data where

import           Data.Default
import           GHC.Generics

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.Minter as MINTA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency     as BBS


data UniversalIndex sft = UniversalIndex
    { minta_untapped_value       :: UntappedValue (SFT_MVAL sft)
    , minta_minted_value         :: MINTA.MintedValue (SFT_MVAL sft)
    , ita_untapped_value         :: UntappedValue (SFT_MVAL sft)
    , cfa_settled_at             :: SFT_TS sft
    , cfa_settled_untapped_value :: UntappedValue (SFT_MVAL sft)
    , cfa_settled_buffer_value   :: BBS.BufferValue (SFT_MVAL sft)
    , cfa_net_flow_rate          :: SFT_MVAL sft
    , dfa_settledAt              :: SFT_TS sft
    , dfa_αVal                   :: SFT_FLOAT sft
    , dfa_εVal                   :: SFT_FLOAT sft
    , dfa_settledBuffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (UniversalIndex sft)
