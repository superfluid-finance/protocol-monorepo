{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MonetaryUnit
    ( MonetaryUnit (..)
    , ProportionalDistributionIndexID
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Kind                                                         (Type)

import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                as UIDX

-- | Monetary unit type class.
--
-- It represents the Superfluid take on the monetary unit in the theory of money distribution.
--
-- TODO: This is should also be a monad.
class SuperfluidTypes sft => MonetaryUnit mu sft | mu -> sft where
    -- * Polymorphic agreement account data functions
    --

    type AnyAgreementMonetaryUnitData mu :: Type

    providedBalanceByAnyAgreement        :: mu -> AnyAgreementMonetaryUnitData mu -> SFT_TS sft -> SFT_RTB sft

    agreementsOf                         :: mu -> [AnyAgreementMonetaryUnitData mu]

    -- * Nomenclature:
    --
    -- Minter = Minter Agreement over the Universal Index
    -- ITA   = Instant Transfer Agreement over the Universal Index
    -- CFA   = Constant Flow Agreement over the Universal Index
    -- DFA   = Decaying Flow Agreement over the Universal Index
    -- IDA   = Instant Distribution Agreement over a Proportional Distribution Index
    -- FDA   = Constant Flow Agreement over a Proportional Distribution Index
    -- DFDA  = Decaying Flow Agreement over a Proportional Distribution Index

    -- * Universal Index Agreement Operations
    --

    -- | Getter of the lenses of monetary unit data in the universal index.
    universalIndex :: SimpleGetter mu (UIDX.UniversalData sft)

    -- | Lens for Minter data in the mu.
    minterMonetaryUnitData :: Lens' mu (UIDX.MinterMonetaryUnitData sft)

    -- | Lens for ITA data in the mu.
    itaMonetaryUnitData :: Lens' mu (UIDX.ITAMonetaryUnitData sft)

    -- | Lens for CFA data in the mu.
    cfaMonetaryUnitData :: Lens' mu (UIDX.CFAMonetaryUnitData sft)

    -- | Lens for DFA data in the mu.
    dfaMonetaryUnitData :: Lens' mu (UIDX.DFAMonetaryUnitData sft)

    -- * Proportional Distribution Index Agreement Operations
    --

    -- | Lens of the lenses of mu in the proportional distribution indexes.
    idaPublisherMonetaryUnitData :: Lens' mu (PDIDX.IDAPublisherMonetaryUnitData sft)

    -- | Getter for the list of IDA subscriber data in the mu.
    idaSubscriberMonetaryUnitDataList :: SimpleGetter mu [PDIDX.IDASubscriberMonetaryUnitData sft]

type ProportionalDistributionIndexID = Int

-- | Calculate the real time balance of an monetary unit at a given time.
balanceOfAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => mu -> SFT_TS sft -> SFT_RTB sft
balanceOfAt mu t = foldr
    ((<>) . (\a -> providedBalanceByAnyAgreement mu a t))
    mempty
    (agreementsOf mu)

-- | Sum the real time balances of a list of accounts at a given time.
sumBalancesAt :: (SuperfluidTypes sft, MonetaryUnit mu sft) => [mu] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((<>) . (`balanceOfAt` t)) mempty alist
