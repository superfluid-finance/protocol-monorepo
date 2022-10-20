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
    , listDistributionContracts
    , listSubscriptionContracts
    ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Binary                                                       (Binary)
import           Data.Char                                                         (isAlpha)
import           Data.Default
import qualified Data.Map                                                          as M
import           Data.Maybe
import           Data.String
import           Data.Type.TaggedTypeable
import           Lens.Internal

import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow            as CFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.DecayingFlow            as DFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.InstantValue            as IVMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue             as MVMUD
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                as UIDX
--
import qualified Money.Systems.Superfluid.Token                                    as SF
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
    { account_data  :: SimpleAccountData
    , subscriptions :: [SimpleSubscriberData]
    }

mk_uidx_mudLs
    :: Lens' SimpleUniversalData mudLs
    -> (mudLs -> mud)
    -> (mud -> mudLs)
    -> Lens' SimpleAccount mud
mk_uidx_mudLs mudLs mkMud getMudLs = lens
    (mkMud . view mudLs . universal_index . account_data)
    (\acc@(SimpleAccount{ account_data = accData@(SimpleAccountData{ universal_index = uidx }) }) mud ->
         let uidx' = set mudLs (getMudLs mud) uidx
         in  acc { account_data = accData { universal_index = uidx' }})

mk_pdidx_pubLs
    :: Lens' SimplePublisherData mudLs
    -> (mudLs -> mud) -> (mud -> mudLs)
    -> Lens' SimpleAccount mud
mk_pdidx_pubLs mudLs mkMud getMudLs = lens
    (mkMud . view mudLs . pd_publisher . account_data)
    (\acc@(SimpleAccount{ account_data = accData@(SimpleAccountData{ pd_publisher = uidx }) }) mud ->
         let pub' = set mudLs (getMudLs mud) uidx
         in  acc { account_data = accData { pd_publisher = pub' }})

instance SF.MonetaryUnit SimpleAccount SimpleSuperfluidSystem where

    monetaryUnitDataList acc =
        [ MkAnySimpleMonetaryUnitData (acc^.SF.minterMonetaryUnitData)
        , MkAnySimpleMonetaryUnitData (acc^.SF.itaMonetaryUnitData)
        , MkAnySimpleMonetaryUnitData (acc^.SF.cfaMonetaryUnitData)
        , MkAnySimpleMonetaryUnitData (acc^.SF.dfaMonetaryUnitData)
        ]
        -- IDA
        <> [MkAnySimpleMonetaryUnitData (acc^.SF.idaPublisherMonetaryUnitData)]
        <> fmap MkAnySimpleMonetaryUnitData (acc^.SF.idaSubscriberMonetaryUnitDataList)
        -- CFDA
        <> [MkAnySimpleMonetaryUnitData (acc^.SF.cfdaPublisherMonetaryUnitData)]
        <> fmap MkAnySimpleMonetaryUnitData (acc^.SF.cfdaSubscriberMonetaryUnitDataList)

    universalData = to (universal_index . account_data)
    minterMonetaryUnitData = mk_uidx_mudLs UIDX.minta_lenses MVMUD.MkMonetaryUnitData MVMUD.getMonetaryUnitLenses
    itaMonetaryUnitData = mk_uidx_mudLs UIDX.ita_lenses IVMUD.MkMonetaryUnitData IVMUD.getMonetaryUnitLenses
    cfaMonetaryUnitData = mk_uidx_mudLs UIDX.cfa_lenses CFMUD.MkMonetaryUnitData CFMUD.getMonetaryUnitLenses
    dfaMonetaryUnitData = mk_uidx_mudLs UIDX.dfa_lenses DFMUD.MkMonetaryUnitData DFMUD.getMonetaryUnitLenses

    pdPublisherData = to (pd_publisher . account_data)
    idaPublisherMonetaryUnitData = mk_pdidx_pubLs
        PDIDX.pub_ida_lenses IVMUD.MkMonetaryUnitData IVMUD.getMonetaryUnitLenses
    idaSubscriberMonetaryUnitDataList = to $
        fmap (IVMUD.MkMonetaryUnitData . PDIDX.ida_sub_data) . subscriptions
    cfdaPublisherMonetaryUnitData = mk_pdidx_pubLs
        PDIDX.pub_cfda_lenses CFMUD.MkMonetaryUnitData CFMUD.getMonetaryUnitLenses
    cfdaSubscriberMonetaryUnitDataList = to $
        fmap (CFMUD.MkMonetaryUnitData . PDIDX.cfda_sub_data) . subscriptions

instance SF.Account SimpleAccount SimpleSuperfluidSystem where
    type ACC_ADDR SimpleAccount = SimpleAddress

showAccountAt :: SimpleAccount -> SimpleTimestamp -> String
showAccountAt acc t =
    "Balance: " ++ show(SF.balanceOfAt acc t) ++
    concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.monetaryUnitDataList acc) ++
    "\nLast Update: " ++ show(account_last_updated_at . account_data $ acc)
    where agreementTypeTag (MkAnySimpleMonetaryUnitData g) = tagFromValue g

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

type CFA_KEY = AgreementOperationOutputF SimpleCFAContractData SimpleAddress

type DFA_KEY = AgreementOperationOutputF SimpleDFAContractData SimpleAddress

data PDPUB_KEY = PDPUB_KEY SimpleAddress SF.ProportionalDistributionIndexID deriving (Show)
instance Eq  PDPUB_KEY where PDPUB_KEY a b == PDPUB_KEY a' b' = a == a' && b == b'
instance Ord PDPUB_KEY where PDPUB_KEY a b <= PDPUB_KEY a' b' = a <= a' && b <= b'

data PDSUB_KEY = PDSUB_KEY SimpleAddress PDPUB_KEY deriving (Show)
instance Eq  PDSUB_KEY where PDSUB_KEY a b == PDSUB_KEY a' b' = a == a' && b == b'
instance Ord PDSUB_KEY where PDSUB_KEY a b <= PDSUB_KEY a' b' = a <= a' && b <= b'

-- | Simple token data type.
data SimpleTokenData = SimpleTokenData
    { accounts               :: M.Map SimpleAddress SimpleAccountData
    , cfaContractData        :: M.Map CFA_KEY   SimpleCFAContractData
    , dfaContractData        :: M.Map DFA_KEY   SimpleDFAContractData
    , distribution_contracts :: M.Map PDPUB_KEY SimpleDistributionContract
    , subscriber_contracts   :: M.Map PDSUB_KEY SimpleSubscriptionContract
    , tokenLastUpdatedAt     :: SimpleTimestamp
    }

instance Default SimpleTokenData where
    def = SimpleTokenData
        { accounts = M.fromList [(minter_address, create_simple_account_data t)]
        , cfaContractData = def
        , dfaContractData = def
        , distribution_contracts = def
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
instance Monad m => SF.Token (SimpleTokenStateT m) SimpleAccount SimpleSuperfluidSystem where

    getCurrentTime = getSystemData <&> currentTime

    getAccount addr = do
        token <- getSimpleTokenData
        let accData = token
                & accounts
                & M.lookup addr
                & fromMaybe (create_simple_account_data 0)
        let pubs = token & distribution_contracts
        let subs = token
                & subscriber_contracts
                & M.filterWithKey (\(PDSUB_KEY s _) _ -> s == addr)
                & M.toList
                & fmap (\(PDSUB_KEY _ k, sub) -> (fromJust $ M.lookup k pubs, sub))
        return $ SimpleAccount { account_data = accData, subscriptions = subs }

    putAccount addr (SimpleAccount { account_data = accData }) t = modify $ \vs -> vs
        { accounts = M.insert addr (accData { account_last_updated_at = t }) (accounts vs) }

    -- * MTA
    --

    getMinterAddress = return minter_address

    view_minter_contract _     = return def
    set_minter_contract  _ _ _ = return ()

    -- * ITA
    --

    view_ita_contract _     = return def
    set_ita_contract  _ _ _ = return ()

    -- * CFA
    --
    calcFlowBuffer = return  . (* Wad 3600)

    view_flow acAddr = getSimpleTokenData
        <&> cfaContractData
        <&> M.lookup acAddr
        <&> fromMaybe def
    set_flow  acAddr ac t = modify $ \vs -> vs
        { cfaContractData = M.insert acAddr ac (cfaContractData vs)
        , tokenLastUpdatedAt = t
        }

    -- * DFA
    --
    view_decaying_flow acAddr = getSimpleTokenData
        <&> dfaContractData
        <&> M.lookup acAddr
        <&> fromMaybe def
    set_decaying_flow  acAddr ac t = modify $ \vs -> vs
        { dfaContractData = M.insert acAddr ac (dfaContractData vs)
        , tokenLastUpdatedAt = t
        }

    -- * PDIDX
    --

    viewProportionalDistributionContract publisher indexId = getSimpleTokenData
        <&> distribution_contracts
        <&> M.lookup (PDPUB_KEY publisher indexId)
        <&> fromMaybe def
    overProportionalDistributionContract publisher indexId updater t = modify $ \vs -> vs
        { distribution_contracts = M.alter
                                   (Just . updater . fromMaybe def)
                                   (PDPUB_KEY publisher indexId)
                                   (distribution_contracts vs)
        , tokenLastUpdatedAt = t
        }

    viewProportionalDistributionSubscription subscriber publisher indexId = getSimpleTokenData
        <&> subscriber_contracts
        <&> M.lookup (PDSUB_KEY subscriber (PDPUB_KEY publisher indexId))
        <&> fromMaybe def
    overProportionalDistributionSubscription subscriber publisher indexId updater t = modify $ \vs -> vs
        { subscriber_contracts = M.alter
                                 (Just . updater . fromMaybe def)
                                 (PDSUB_KEY subscriber (PDPUB_KEY publisher indexId))
                                 (subscriber_contracts vs)
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

listDistributionContracts :: Monad m => SimpleTokenStateT m [(PDPUB_KEY, SimpleDistributionContract)]
listDistributionContracts = getSimpleTokenData <&> M.toList . distribution_contracts

listSubscriptionContracts :: Monad m => SimpleTokenStateT m [(PDSUB_KEY, SimpleSubscriptionContract)]
listSubscriptionContracts = getSimpleTokenData <&> M.toList . subscriber_contracts
