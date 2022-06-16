{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}

module Money.Superfluid.Concepts.Agreement
    ( AgreementContractData
    , AgreementAccountData (..)
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceOfAnyAgreement
    , agreementTypeTag
    ) where

import           Data.Internal.TaggedTypeable              (TaggedTypeable (tagFromType))

import           Money.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))

-- ============================================================================
-- | AgreementContractData type class
--
-- Naming conventions:
--  * Type name: acd
class (TaggedTypeable acd, Show acd, SuperfluidTypes sft)
    => AgreementContractData acd sft | acd -> sft where

-- ============================================================================
-- | AgreementAccountData type class
--
-- Naming conventions:
--  - Type name: aad
class (TaggedTypeable aad, Show aad, SuperfluidTypes sft)
    => AgreementAccountData aad sft | aad -> sft where
    providedBalanceOfAgreement :: aad -> SFT_TS sft -> SFT_RTB sft

-- ============================================================================
-- | AnyAgreementAccountData type
--
-- Naming conventions:
--  - Type name: aaad
--  - Term name: anyAgreement
--
-- Notes:
-- - To Enumerate all supported agreements using GADTs
--   See: https://wiki.haskell.org/Heterogenous_collections
-- - MkAgreementAccountData is the constructor
-- - providedBalanceOfAnyAgreement is convenience wrapper of providedBalanceOfAgreement
data AnyAgreementAccountData sft where
    MkAgreementAccountData
        :: (SuperfluidTypes sft, AgreementAccountData aad sft)
        => aad -> AnyAgreementAccountData sft

instance Show (AnyAgreementAccountData sft) where
    show (MkAgreementAccountData g) = show g

-- | providedBalanceOfAgreement wrapper for AnyAgreementAccountData
providedBalanceOfAnyAgreement :: AnyAgreementAccountData sft -> SFT_TS sft -> SFT_RTB sft
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOfAgreement g

-- | agreementTypeTag
agreementTypeTag :: AnyAgreementAccountData sft -> String
agreementTypeTag (MkAgreementAccountData g) = tagFromType g
