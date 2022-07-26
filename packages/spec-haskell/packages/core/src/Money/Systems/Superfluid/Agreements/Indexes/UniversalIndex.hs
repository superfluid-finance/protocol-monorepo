{-# LANGUAGE DeriveAnyClass #-}

module Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex where

import           Data.Default
import           GHC.Generics

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue as MVMUD
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency     as BBS


-- | This is data that is universally available to the monetary unit.
--
-- NOTE: It is called universal index because of this reason: it is a trivial index
-- for the monetary unit to access the universal data.
data UniversalData sft = UniversalData -- UniversalMonetaryUnitData
    { minter_untapped_value      :: UntappedValue (SFT_MVAL sft)
    , minter_minted_value        :: MVMUD.MintedValue (SFT_MVAL sft)
    , ita_untapped_value         :: UntappedValue (SFT_MVAL sft)
    , cfa_settled_at             :: SFT_TS sft
    , cfa_settled_untapped_value :: UntappedValue (SFT_MVAL sft)
    , cfa_settled_buffer_value   :: BBS.BufferValue (SFT_MVAL sft)
    , cfa_net_flow_rate          :: SFT_MVAL sft
    , dfa_settledAt              :: SFT_TS sft -- FIXME
    , dfa_αVal                   :: SFT_FLOAT sft
    , dfa_εVal                   :: SFT_FLOAT sft
    , dfa_settledBuffer          :: BBS.BufferValue (SFT_MVAL sft)
    } deriving (Generic)
deriving instance SuperfluidTypes sft => Default (UniversalData sft)
