{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

module Money.Systems.Superfluid.Instances.Simple.System
    ( module Money.Systems.Superfluid.Instances.Simple.Types
    -- SimpleAccount
    , SimpleAddress (..)
    , createSimpleAddress
    , SF.MonetaryUnit (..)
    , SF.Account (..)
    , SF.balanceOfAt
    , SF.sumBalancesAt
    , SimpleAccount
    , showAccountAt
    -- Token
    , SimpleSystemData (..)
    , SimpleTokenData
    , SimpleTokenStateT
    , runSimpleTokenStateT
    , evalSimpleTokenStateT
    , execSimpleTokenStateT
    , getSimpleTokenData
    , SF.Token (..)
    , initSimpleToken
    , addAccount
    , listAccounts
    , listCFAContracts
    , listDFAContracts
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Binary                                                          (Binary)
import           Data.Char                                                            (isAlpha)
import           Data.Default
import           Data.Functor
import qualified Data.Map                                                             as M
import           Data.Maybe
import           Data.String
import           Data.Type.TaggedTypeable
import           Lens.Internal

import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow    as CFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow    as DFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantTransfer as ITMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.Minter          as MMUD
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex    as PDIDX
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                   as UIDX
--
import qualified Money.Systems.Superfluid.Token                                       as SF
--
import           Money.Systems.Superfluid.Instances.Simple.Types


-- | SimpleAddress type.
--
-- Note: It must consist of only alphabetical letters
--
newtype SimpleAddress = SimpleAddress String
    deriving newtype (Eq, Ord, Binary, Show, SF.Address)

instance IsString SimpleAddress where
    fromString = fromJust . createSimpleAddress

-- | SimpleAddress public constructor.
createSimpleAddress :: String -> Maybe SimpleAddress
createSimpleAddress a = if isValidAddress a then Just $ SimpleAddress a else Nothing
    where isValidAddress = all (\x -> any ($ x) [isAlpha, (== '_')])

-- | Simple account type.
data SimpleAccount = SimpleAccount
    { universal_index         :: UIDX.UniversalData SimpleSuperfluidTypes
    , pd_publisher            :: PDIDX.IDAPublisherMonetaryUnitData SimpleSuperfluidTypes
    , pd_subscriptions        :: [PDIDX.SubscriptionContract SimpleSuperfluidTypes]
    , account_last_updated_at :: SimpleTimestamp
    }

instance SF.MonetaryUnit SimpleAccount SimpleSuperfluidTypes where
    type AnyAgreementMonetaryUnitData SimpleAccount = AnySimpleAgreementMonetaryUnitData
    providedBalanceByAnyAgreement _ (MkSimpleAgreementMonetaryUnitData g) = balanceProvidedByAgreement g

    agreementsOf acc = [ MkSimpleAgreementMonetaryUnitData (acc^.SF.minterMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.itaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.cfaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.dfaMonetaryUnitData)
                       ]
                       ++ [MkSimpleAgreementMonetaryUnitData (acc^.SF.proportionalDistributionPublisherData)]
                       ++ fmap MkSimpleAgreementMonetaryUnitData (acc^.SF.idaSubscriberMonetaryUnitDataList)

    universalIndex = $(field 'universal_index)
    minterMonetaryUnitData = lens
        (MMUD.MkMonetaryUnitData . universal_index)
        (\acc (MMUD.MkMonetaryUnitData amud) -> acc { universal_index = amud })
    itaMonetaryUnitData = lens
        (ITMUD.MkMonetaryUnitData . universal_index)
        (\acc (ITMUD.MkMonetaryUnitData amud) -> acc { universal_index = amud })
    cfaMonetaryUnitData = lens
        (CFMUD.MkMonetaryUnitData . universal_index)
        (\acc (CFMUD.MkMonetaryUnitData amud) -> acc { universal_index = amud })
    dfaMonetaryUnitData = lens
        (DFMUD.MkMonetaryUnitData . universal_index)
        (\acc (DFMUD.MkMonetaryUnitData amud) -> acc { universal_index = amud })

    proportionalDistributionPublisherData = $(field 'pd_publisher)
    proportionalDistributionSubscriptions = $(field 'pd_subscriptions)

    idaSubscriberMonetaryUnitDataList = lens (const []) const

instance SF.Account SimpleAccount SimpleSuperfluidTypes where
    type ACC_ADDR SimpleAccount = SimpleAddress

showAccountAt :: SimpleAccount -> SimpleTimestamp -> String
showAccountAt acc t =
    "Balance: " ++ show(SF.balanceOfAt acc t) ++
    concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.agreementsOf acc) ++
    "\nLast Update: " ++ show(account_last_updated_at acc)
    where agreementTypeTag (MkSimpleAgreementMonetaryUnitData g) = tagFromValue g

create_simple_account :: SimpleTimestamp -> SimpleAccount
create_simple_account t = SimpleAccount
    { universal_index = def
    , pd_publisher = def
    , pd_subscriptions = []
    , account_last_updated_at = t
    }

-- | Simple system data type.
newtype SimpleSystemData = SimpleSystemData
    { currentTime   :: SimpleTimestamp
    }

-- | Agreement contract data key using addresses.
type ACD_KEY ao = AgreementOperationPartiesF ao SimpleAddress

-- | Simple token data type.
data SimpleTokenData = SimpleTokenData
    { accounts           :: M.Map SimpleAddress SimpleAccount
    , cfaContractData    :: M.Map (ACD_KEY SimpleCFAOperation) SimpleCFAContractData
    , dfaContractData    :: M.Map (ACD_KEY SimpleDFAOperation) SimpleDFAContractData
    , tokenLastUpdatedAt :: SimpleTimestamp
    }

instance Default SimpleTokenData where
    def = SimpleTokenData
        { accounts = M.fromList [(minter_address, create_simple_account t)]
        , cfaContractData = def
        , dfaContractData = def
        , tokenLastUpdatedAt = t }
        where t = def :: SimpleTimestamp

minter_address :: SimpleAddress
minter_address = "_minter"

-- | Monad transformer stack for the simple token state
type SimpleTokenStateT m = (StateT  SimpleTokenData
                            (ReaderT SimpleSystemData
                             m))

getSystemData :: Monad m => SimpleTokenStateT m SimpleSystemData
getSystemData = lift ask

getSimpleTokenData :: Monad m => SimpleTokenStateT m SimpleTokenData
getSimpleTokenData = get

runSimpleTokenStateT :: SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m (a, SimpleTokenData)
runSimpleTokenStateT m sys token = let m'  = runStateT  m  token
                                       m'' = runReaderT m' sys
                                   in  m''

evalSimpleTokenStateT :: Monad m => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m a
evalSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> fst

execSimpleTokenStateT :: Monad m => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m SimpleTokenData
execSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> snd

-- | SimpleTokenStateT m is a @SF.Token@ instance.
instance Monad m => SF.Token (SimpleTokenStateT m) SimpleAccount SimpleSuperfluidTypes where

    getCurrentTime = getSystemData <&> currentTime

    getAccount addr     = getSimpleTokenData <&> accounts <&> M.lookup addr <&> fromMaybe (create_simple_account 0)
    putAccount addr acc t = modify $ \vs -> vs
        { accounts = M.insert addr (acc { account_last_updated_at = t }) (accounts vs)
        }

    -- * MMUD
    --

    getMinterAddress = return minter_address

    viewMinterContract _     = return $ Just def
    setMinterContract  _ _ _ = return ()

    -- * ITMUD
    --

    viewITAContract _     = return $ Just def
    setITAContract  _ _ _ = return ()

    -- * CFMUD
    --
    calcFlowBuffer = return  . (* Wad 3600)

    viewFlow acdAddr       = getSimpleTokenData <&> cfaContractData <&> M.lookup acdAddr
    setFlow  acdAddr acd t = modify $ \vs -> vs
        { cfaContractData = M.insert acdAddr acd (cfaContractData vs)
        , tokenLastUpdatedAt = t
        }

    -- * DFMUD
    --
    viewDecayingFlow acdAddr       = getSimpleTokenData <&> dfaContractData <&> M.lookup acdAddr
    setDecayingFlow  acdAddr acd t = modify $ \vs -> vs
        { dfaContractData = M.insert acdAddr acd (dfaContractData vs)
        , tokenLastUpdatedAt = t
        }

-- ============================================================================
-- Other SimpleTokenStateT Operations
--
initSimpleToken :: Monad m => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    put SimpleTokenData
        { accounts = M.fromList $ map (\a -> (a, create_simple_account t)) alist
        , cfaContractData = M.empty
        , dfaContractData = M.empty
        , tokenLastUpdatedAt = t
        }
    mapM_ (`SF.mintValue` initBalance) alist

addAccount :: Monad m => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accountAddr account = modify (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: Monad m => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = getSimpleTokenData <&> M.toList . accounts

listCFAContracts :: Monad m => SimpleTokenStateT m [(ACD_KEY SimpleCFAOperation, SimpleCFAContractData)]
listCFAContracts = getSimpleTokenData <&> M.toList . cfaContractData

listDFAContracts :: Monad m => SimpleTokenStateT m [(ACD_KEY SimpleDFAOperation, SimpleDFAContractData)]
listDFAContracts = getSimpleTokenData <&> M.toList . dfaContractData
