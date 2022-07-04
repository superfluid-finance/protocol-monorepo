{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , updateAgreement
    ) where

import           Data.Kind                                         (Type)
import           Data.Type.TaggedTypeable                          (TaggedTypeable (..))

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))


type SFT a = DistributionForAgreement a

-- | Agreement type class
class ( SuperfluidTypes (DistributionForAgreement a)
      , TaggedTypeable (AgreementContractData a)
      , Semigroup (AgreementAccountData a)
      , TaggedTypeable (AgreementAccountData a)
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

    -- | Create an union of two sets of agreement parties with a binary function
    unionAgreementPartiesWith
        :: (AgreementAccountData a -> AgreementAccountData a -> AgreementAccountData a)
        -> AgreementParties a -> AgreementParties a -> AgreementParties a

-- | Update the data of parties of an agreement from the changes of the agreement contract
updateAgreement :: Agreement a => AgreementContractData a -> AgreementContractData a -> AgreementParties a -> AgreementParties a
updateAgreement old new parties = unionAgreementPartiesWith (<>) parties parties'
    where parties' = createAgreementParties old new
