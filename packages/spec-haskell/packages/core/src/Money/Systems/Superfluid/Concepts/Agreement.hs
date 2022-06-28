{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceByAnyAgreement
    , agreementTypeTag
    ) where

import           Data.Default                                      (Default)
import           Data.Internal.TaggedTypeable                      (TaggedTypeable (..), tagFromValue)
import           Data.Kind                                         (Type)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


type SFT a = DistributionForAgreement a

-- | Agreement type class
class ( SuperfluidTypes (DistributionForAgreement a)
      , Default (AgreementContractData a)
      , TaggedTypeable (AgreementContractData a)
      , Monoid (AgreementAccountData a)
      , TaggedTypeable (AgreementAccountData a)
      , Show (AgreementAccountData a)
      )
      => Agreement a where

    type DistributionForAgreement a :: Type

    data AgreementContractData a :: Type
    data AgreementAccountData a :: Type
    data AgreementParties a :: Type

    -- | Balance provided by the agreement of an account
    providedBalanceByAgreement :: AgreementAccountData a -> SFT_TS (SFT a) -> SFT_RTB (SFT a)

    -- | Create data of agreement parties from the changes of the agreement contract
    createAgreementParties :: AgreementContractData a -> AgreementContractData a -> AgreementParties a

    -- | Lift a binary function to the data of two sets of agreement parties
    liftAgreementParties2
        :: (AgreementAccountData a -> AgreementAccountData a -> AgreementAccountData a)
        -> AgreementParties a -> AgreementParties a -> AgreementParties a

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a => AgreementContractData a -> AgreementContractData a -> AgreementParties a -> AgreementParties a
updateAgreement old new parties = liftAgreementParties2 (<>) parties parties'
    where parties' = createAgreementParties old new

-- | AnyAgreementAccountData type
data AnyAgreementAccountData sft =
    forall a. (SuperfluidTypes sft, Agreement a, SFT a ~ sft)
    => MkAgreementAccountData (AgreementAccountData a)

-- | Balance provided by any agreement of an account
providedBalanceByAnyAgreement :: AnyAgreementAccountData sft -> SFT_TS sft -> SFT_RTB sft
providedBalanceByAnyAgreement (MkAgreementAccountData g) = providedBalanceByAgreement g

-- | Get agreement type tag
agreementTypeTag :: AnyAgreementAccountData sft -> String
agreementTypeTag (MkAgreementAccountData g) = tagFromValue g
