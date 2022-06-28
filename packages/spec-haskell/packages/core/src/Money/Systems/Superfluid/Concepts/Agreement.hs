{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( AgreementAccountData (..)
    , AgreementContractData
    , Agreement (..)
    , updateAgreement
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceOfAnyAgreement
    , agreementTypeTag
    ) where

import           Data.Default                                      (Default)
import           Data.Internal.TaggedTypeable                      (TaggedTypeable (..), tagFromValue)
import           Data.Kind                                         (Type)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


-- | AgreementAccountData type class
--
-- Naming conventions:
--   * Type name: aad
class (TaggedTypeable aad, Show aad, SuperfluidTypes sft)
    => AgreementAccountData aad sft | aad -> sft where
    providedBalanceOfAgreement :: aad -> SFT_TS sft -> SFT_RTB sft

-- | AgreementContractData type class
--
-- Naming conventions:
--   * Type name: acd
class (TaggedTypeable acd, Show acd, SuperfluidTypes sft, AgreementAccountData aad sft)
    => AgreementContractData acd sft aad | acd -> sft, acd -> aad where

type SFT a = DistributionForAgreement a

-- | Agreement type class
class ( SuperfluidTypes (DistributionForAgreement a)
      , Default (AgreementContractData' a)
      , Monoid (AgreementAccountData' a)
      )
      => Agreement a where

    type DistributionForAgreement a :: Type

    data AgreementContractData' a :: Type
    data AgreementAccountData' a :: Type
    data AgreementParties a :: Type

    -- | Balance provided by the agreement of an account
    providedBalanceByAgreement :: AgreementAccountData' a -> SFT_TS (SFT a) -> SFT_RTB (SFT a)

    -- | Create data of agreement parties from the changes of the agreement contract
    createAgreementParties :: AgreementContractData' a -> AgreementContractData' a -> AgreementParties a

    -- | Lift a binary function to the data of two sets of agreement parties
    liftAgreementParties2
        :: (AgreementAccountData' a -> AgreementAccountData' a -> AgreementAccountData' a)
        -> AgreementParties a -> AgreementParties a -> AgreementParties a

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a => AgreementContractData' a -> AgreementContractData' a -> AgreementParties a -> AgreementParties a
updateAgreement old new parties = liftAgreementParties2 (<>) parties parties'
    where parties' = createAgreementParties old new

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

-- | providedBalanceOfAgreement wrapper for AnyAgreementAccountData
providedBalanceOfAnyAgreement :: AnyAgreementAccountData sft -> SFT_TS sft -> SFT_RTB sft
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOfAgreement g

-- | Get agreement type tag
agreementTypeTag :: AnyAgreementAccountData sft -> String
agreementTypeTag (MkAgreementAccountData g) = tagFromValue g
