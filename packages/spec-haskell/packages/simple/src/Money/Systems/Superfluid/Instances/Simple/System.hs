{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , listPublisherContracts
    , listIDASubscriptionContracts
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Binary                                                               (Binary)
import           Data.Char                                                                 (isAlpha)
import           Data.Default
import           Data.Functor
import qualified Data.Map                                                                  as M
import           Data.Maybe
import           Data.String
import           Data.Type.TaggedTypeable
import           Lens.Internal

import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.ConstantFlow         as CFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.DecayingFlow         as DFMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.InstantValue         as ITMUD
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue          as MMUD
--
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
--
import qualified Money.Systems.Superfluid.Token                                            as SF
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

-- | Simple account data.
data SimpleAccountData = SimpleAccountData
    { universal_index         :: SimpleUniversalData
    , pd_publisher            :: SimplePublisherData
    , account_last_updated_at :: SimpleTimestamp
    }

-- | Simple account information.
--
-- PD subscriptions are loaded on demand.
data SimpleAccount = SimpleAccount
    { account_data      :: SimpleAccountData
    , ida_subscriptions :: [SimpleSubscriberData]
    }

mk_uidx_mud_lens
    :: (SimpleUniversalData -> amud)
    -> (amud -> SimpleUniversalData)
    -> Lens' SimpleAccount amud
mk_uidx_mud_lens mkMud getMudL = lens
    (mkMud . universal_index . account_data)
    (\acc@(SimpleAccount{ account_data = accData }) amud ->
         acc { account_data = accData { universal_index = getMudL amud } })

mk_pdidx_mud_lens
    :: (SimplePublisherData -> amud)
    -> (amud -> SimplePublisherData)
    -> Lens' SimpleAccount amud
mk_pdidx_mud_lens mkMud getMudL = lens
    (mkMud . pd_publisher . account_data)
    (\acc@(SimpleAccount{ account_data = accData }) amud ->
         acc { account_data = accData { pd_publisher = getMudL amud } })

instance SF.MonetaryUnit SimpleAccount SimpleSuperfluidTypes where
    type AnyAgreementMonetaryUnitData SimpleAccount = AnySimpleAgreementMonetaryUnitData

    providedBalanceByAnyAgreement _ (MkSimpleAgreementMonetaryUnitData g) = balanceProvidedByAgreement g

    agreementsOf acc = [ MkSimpleAgreementMonetaryUnitData (acc^.SF.minterMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.itaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.cfaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.dfaMonetaryUnitData)
                       ]
                       ++ [MkSimpleAgreementMonetaryUnitData (acc^.SF.idaPublisherMonetaryUnitData)]
                       ++ fmap MkSimpleAgreementMonetaryUnitData (acc^.SF.idaSubscriberMonetaryUnitDataList)

    universalData = to (universal_index . account_data)
    minterMonetaryUnitData = mk_uidx_mud_lens MMUD.MkMonetaryUnitData MMUD.getMonetaryUnitLenses
    itaMonetaryUnitData    = mk_uidx_mud_lens ITMUD.MkMonetaryUnitData ITMUD.getMonetaryUnitLenses
    cfaMonetaryUnitData    = mk_uidx_mud_lens CFMUD.MkMonetaryUnitData CFMUD.getMonetaryUnitLenses
    dfaMonetaryUnitData    = mk_uidx_mud_lens DFMUD.MkMonetaryUnitData DFMUD.getMonetaryUnitLenses

    pdPublisherData = to (pd_publisher . account_data)
    idaPublisherMonetaryUnitData = mk_pdidx_mud_lens ITMUD.MkMonetaryUnitData ITMUD.getMonetaryUnitLenses
    idaSubscriberMonetaryUnitDataList = to (fmap ITMUD.MkMonetaryUnitData . ida_subscriptions)

instance SF.Account SimpleAccount SimpleSuperfluidTypes where
    type ACC_ADDR SimpleAccount = SimpleAddress

showAccountAt :: SimpleAccount -> SimpleTimestamp -> String
showAccountAt acc t =
    "Balance: " ++ show(SF.balanceOfAt acc t) ++
    concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.agreementsOf acc) ++
    "\nLast Update: " ++ show(account_last_updated_at . account_data $ acc)
    where agreementTypeTag (MkSimpleAgreementMonetaryUnitData g) = tagFromValue g

create_simple_account_data :: SimpleTimestamp -> SimpleAccountData
create_simple_account_data t = SimpleAccountData
    { universal_index = def
    , pd_publisher = def
    , account_last_updated_at = t
    }

-- | Simple system data type.
newtype SimpleSystemData = SimpleSystemData
    { currentTime   :: SimpleTimestamp
    }

type CFA_KEY = AgreementOperationResultF SimpleCFAOperation SimpleAddress

type DFA_KEY = AgreementOperationResultF SimpleDFAOperation SimpleAddress

data PDPUB_KEY = PDPUB_KEY SimpleAddress SF.ProportionalDistributionIndexID deriving (Show)
instance Eq  PDPUB_KEY where PDPUB_KEY a b == PDPUB_KEY a' b' = a == a' && b == b'
instance Ord PDPUB_KEY where PDPUB_KEY a b <= PDPUB_KEY a' b' = a <= a' && b <= b'

data PDSUB_KEY = PDSUB_KEY SimpleAddress PDPUB_KEY deriving (Show)
instance Eq  PDSUB_KEY where PDSUB_KEY a b == PDSUB_KEY a' b' = a == a' && b == b'
instance Ord PDSUB_KEY where PDSUB_KEY a b <= PDSUB_KEY a' b' = a <= a' && b <= b'

-- | Simple token data type.
data SimpleTokenData = SimpleTokenData
    { accounts             :: M.Map SimpleAddress SimpleAccountData
    , cfaContractData      :: M.Map CFA_KEY   SimpleCFAContractData
    , dfaContractData      :: M.Map DFA_KEY   SimpleDFAContractData
    , publisher_contracts  :: M.Map PDPUB_KEY SimpleDistributionContract
    , subscriber_contracts :: M.Map PDSUB_KEY SimpleSubscriptionContract
    , tokenLastUpdatedAt   :: SimpleTimestamp
    }

instance Default SimpleTokenData where
    def = SimpleTokenData
        { accounts = M.fromList [(minter_address, create_simple_account_data t)]
        , cfaContractData = def
        , dfaContractData = def
        , publisher_contracts = def
        , subscriber_contracts = def
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

    getAccount addr = do
        token <- getSimpleTokenData
        let accData = token
                & accounts
                & M.lookup addr
                & fromMaybe (create_simple_account_data 0)
        let pubs = token & publisher_contracts
        let ida_subs = token
                & subscriber_contracts
                & M.filterWithKey (\(PDSUB_KEY s _) _ -> s == addr)
                & M.toList
                & fmap (\(PDSUB_KEY _ k, sub) -> PDIDX.SubscriberData
                           (fromJust $ M.lookup k pubs)
                           sub)
        return $ SimpleAccount { account_data = accData, ida_subscriptions = ida_subs }

    putAccount addr (SimpleAccount { account_data = accData }) t = modify $ \vs -> vs
        { accounts = M.insert addr (accData { account_last_updated_at = t }) (accounts vs) }

    -- * MTA
    --

    getMinterAddress = return minter_address

    viewMinterContract _     = return $ Just def
    setMinterContract  _ _ _ = return ()

    -- * ITA
    --

    viewITAContract _     = return $ Just def
    setITAContract  _ _ _ = return ()

    -- * CFA
    --
    calcFlowBuffer = return  . (* Wad 3600)

    viewFlow acdAddr       = getSimpleTokenData
        <&> cfaContractData
        <&> M.lookup acdAddr
    setFlow  acdAddr acd t = modify $ \vs -> vs
        { cfaContractData = M.insert acdAddr acd (cfaContractData vs)
        , tokenLastUpdatedAt = t
        }

    -- * DFA
    --
    viewDecayingFlow acdAddr       = getSimpleTokenData
        <&> dfaContractData
        <&> M.lookup acdAddr
    setDecayingFlow  acdAddr acd t = modify $ \vs -> vs
        { dfaContractData = M.insert acdAddr acd (dfaContractData vs)
        , tokenLastUpdatedAt = t
        }

    -- * IDA
    --

    viewProportionalDistributionContract publisher indexId = getSimpleTokenData
        <&> publisher_contracts
        <&> M.lookup (PDPUB_KEY publisher indexId)

    setProportionalDistributionContract publisher indexId index t = modify $ \vs -> vs
        { publisher_contracts = M.insert (PDPUB_KEY publisher indexId) index (publisher_contracts vs)
        , tokenLastUpdatedAt = t
        }

    viewProportionalDistributionSubscription subscriber publisher indexId = getSimpleTokenData
        <&> subscriber_contracts
        <&> M.lookup (PDSUB_KEY subscriber (PDPUB_KEY publisher indexId))

    setProportionalDistributionSubscription subscriber publisher indexId sub t = modify $ \vs -> vs
        { subscriber_contracts = M.insert (PDSUB_KEY subscriber (PDPUB_KEY publisher indexId))
                                 sub (subscriber_contracts vs)
        , tokenLastUpdatedAt = t
        }

-- ============================================================================
-- Other SimpleTokenStateT Operations
--
initSimpleToken :: Monad m => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    put def
        { accounts = M.fromList $ map (, create_simple_account_data t) alist
        , tokenLastUpdatedAt = t
        }
    mapM_ (`SF.mintValue` initBalance) alist

addAccount :: Monad m => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accAddr (SimpleAccount { account_data = accData }) = modify (\vs -> vs {
    accounts = M.insert accAddr accData (accounts vs)})

listAccounts :: Monad m => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = let allAddrs = getSimpleTokenData <&> accounts <&> M.keys
               -- I mean, just read the type signature.
               -- Since it compiles, it must does what it says here, you can guess.
               in mapM (\addr -> (addr,) <$> SF.getAccount addr) =<< allAddrs

listCFAContracts :: Monad m => SimpleTokenStateT m [(CFA_KEY, SimpleCFAContractData)]
listCFAContracts = getSimpleTokenData <&> M.toList . cfaContractData

listDFAContracts :: Monad m => SimpleTokenStateT m [(DFA_KEY, SimpleDFAContractData)]
listDFAContracts = getSimpleTokenData <&> M.toList . dfaContractData

listPublisherContracts :: Monad m => SimpleTokenStateT m [(PDPUB_KEY, SimpleDistributionContract)]
listPublisherContracts = getSimpleTokenData <&> M.toList . publisher_contracts

listIDASubscriptionContracts :: Monad m => SimpleTokenStateT m [(PDSUB_KEY, SimpleSubscriptionContract)]
listIDASubscriptionContracts = getSimpleTokenData <&> M.toList . subscriber_contracts
