{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.MoneyUnit
    ( MoneyUnit (..)
    , balanceOfAt
    , sumBalancesAt
    ) where

import           Data.Default                                     (Default (def))
import           Data.Kind                                        (Type)
import           Lens.Micro

import           Money.Systems.Superfluid.Concepts                (SuperfluidTypes (..))
--
import qualified Money.Systems.Superfluid.Indexes.Universalndexes as UIDX

-- | MoneyUnit type class.
class SuperfluidTypes sft => MoneyUnit mu sft | mu -> sft where
    -- * Polymorphic agreement account data functions
    --

    type AnyAgreementMonetaryUnitData mu :: Type

    agreementsOf                         :: mu -> [AnyAgreementMonetaryUnitData mu]

    providedBalanceByAnyAgreement        :: mu -> AnyAgreementMonetaryUnitData mu -> SFT_TS sft -> SFT_RTB sft

    -- * Lenses of agreement monetary unit data
    --

    tbaMonetaryUnitLens :: Lens' mu (UIDX.TBAMonetaryUnitData sft)
    -- idaLens  :: Lens' mu PDIDX.TBAMonetaryUnitData
    -- qidaLens :: Lens' mu QDIDX.TBAMonetaryUnitData

    cfaMonetaryUnitLens :: Lens' mu (UIDX.CFAMonetaryUnitData sft)
    -- fdaLens  :: Lens' mu PDIDX.CFAMonetaryUnitData
    -- qfdaLens :: Lens' mu QDIDX.CFAMonetaryUnitData

    dfaMonetaryUnitLens :: Lens' mu (UIDX.DFAMonetaryUnitData sft)
    -- pdfaLens :: Lens' mu PDIDX.DFAMonetaryUnitData
    -- qdfaLens :: Lens' mu QDIDX.DFAMonetaryUnitData

balanceOfAt :: (SuperfluidTypes sft, MoneyUnit mu sft) => mu -> SFT_TS sft -> SFT_RTB sft
balanceOfAt account t = foldr
    ((+) . (\a -> providedBalanceByAnyAgreement account a t))
    def
    (agreementsOf account)

sumBalancesAt :: (SuperfluidTypes sft, MoneyUnit mu sft) => [mu] -> SFT_TS sft -> SFT_RTB sft
sumBalancesAt alist t = foldr ((+) . (`balanceOfAt` t)) def alist
