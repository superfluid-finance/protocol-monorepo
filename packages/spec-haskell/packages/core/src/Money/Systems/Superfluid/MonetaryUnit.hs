{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MonetaryUnit
    ( MonetaryUnit (..)
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Kind                                                                 (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement                 as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement                 as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantDistributionAgreement          as IDA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement              as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement                       as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Agreements.Indexes.UniversalIndex                as UIDX

-- | Monetary unit type class.
--
-- It represents the Superfluid take on the monetary unit in the theory of money distribution.
class SuperfluidTypes sft => MonetaryUnit mu sft | mu -> sft where
    -- * Polymorphic agreement account data functions
    --

    type AnyAgreementMonetaryUnitData mu :: Type

    providedBalanceByAnyAgreement        :: mu -> AnyAgreementMonetaryUnitData mu -> SFT_TS sft -> SFT_RTB sft

    agreementsOf                         :: mu -> [AnyAgreementMonetaryUnitData mu]

    -- * Nomenclature:
    --
    -- MINTA = Minter Agreement over the Universal Index
    -- ITA   = Instant Transfer Agreement over the Universal Index
    -- CFA   = Constant Flow Agreement over the Universal Index
    -- DFA   = Decaying Flow Agreement over the Universal Index
    -- IDA   = Instant Distribution Agreement over a Proportional Distribution Index
    -- FDA   = Constant Flow Agreement over a Proportional Distribution Index
    -- DFDA  = Decaying Flow Agreement over a Proportional Distribution Index

    -- * Universal Index Agreement Operations
    --

    -- | Getter for the lenses of monetary unit data in the universal index.
    universalData :: SimpleGetter mu (UIDX.UniversalData sft)

    -- | Lens for Minter data.
    minterMonetaryUnitData :: Lens' mu (MINTA.MonetaryUnitData sft)

    -- | Lens for ITA data.
    itaMonetaryUnitData :: Lens' mu (ITA.MonetaryUnitData sft)

    -- | Lens for CFA data.
    cfaMonetaryUnitData :: Lens' mu (CFA.MonetaryUnitData sft)

    -- | Lens for DFA data.
    dfaMonetaryUnitData :: Lens' mu (DFA.MonetaryUnitData sft)

    -- * Proportional Distribution Index Agreement Operations
    --

    -- | Getter for the publisher lenses of monetary unit data in the proportional distribution (PD) index.
    pdPublisherData :: SimpleGetter mu (PDIDX.PublisherData sft)

    -- | Getter for the list of subscriber IDA data.
    idaSubscriberMonetaryUnitDataList :: SimpleGetter mu [IDA.IDASubscriberMonetaryUnitData sft]

    -- | Lens for the publisher IDA data.
    idaPublisherMonetaryUnitData :: Lens' mu (IDA.IDAPublisherMonetaryUnitData sft)

-- | Calculate the real time balance of an monetary unit at a given time.
balanceOfAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => mu -> SFT_TS sft -> SFT_RTB sft
balanceOfAt mu t = foldr
    ((<>) . (flip (providedBalanceByAnyAgreement mu) t))
    mempty
    (agreementsOf mu)

-- | Sum the real time balances of a list of accounts at a given time.
sumBalancesAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => [mu] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((<>) . (`balanceOfAt` t)) mempty alist
