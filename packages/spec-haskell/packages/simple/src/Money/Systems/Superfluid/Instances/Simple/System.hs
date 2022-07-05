{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Money.Systems.Superfluid.Instances.Simple.System
    ( module Money.Systems.Superfluid.Instances.Simple.Types
    -- SimpleAccount
    , SF.Account (..)
    , SF.balanceOfAccountAt
    , SF.sumAccounts
    , SimpleAccount
    , showAccountAt
    , listAccounts
    , addAccount
    -- Token
    , SimpleSystemData (..)
    , SimpleTokenData
    , SimpleTokenStateT
    , runSimpleTokenStateT
    , evalSimpleTokenStateT
    , execSimpleTokenStateT
    , getSimpleTokenData
    , SF.Token (..)
    , initSimpleToken)
    where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Default
import           Data.Functor
import qualified Data.Map                                                         as M
import           Data.Maybe
import           Data.Type.TaggedTypeable

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.System.AccountTokenModel                as SF

import           Money.Systems.Superfluid.Instances.Simple.Types


-- ============================================================================
-- SimpleAccount Type and Operations (is SuperfluidAccount)
--
data SimpleAccount = SimpleAccount
    { address              :: SimpleAddress
    , tbaAccountData       :: TBA.TBAAccountData SimpleSuperfluidDistribution
    , cfaAccountData       :: CFA.CFAAccountData SimpleSuperfluidDistribution
    , dfaAccountData       :: DFA.DFAAccountData SimpleSuperfluidDistribution
    , accountLastUpdatedAt :: SimpleTimestamp
    }

-- agreement_of_Account
--   :: (Agreement a, S.Serializable (AgreementAccountData a) SimpleSuperfluidDistribution)
--    => Proxy (AgreementAccountData a) -> SimpleAccount -> Maybe (AgreementAccountData a)

instance SF.Account SimpleAccount SimpleSuperfluidDistribution where
    addressOfAccount = address

    type AnyAgreementAccountData SimpleAccount = AnySimpleAgreementAccountData
    agreementsOfAccount acc = [ MkSimpleAgreementAccountData (SF.viewAccountTBA acc)
                              , MkSimpleAgreementAccountData (SF.viewAccountCFA acc)
                              , MkSimpleAgreementAccountData  (SF.viewAccountDFA acc)
                              ]
    providedBalanceByAnyAgreement _ (MkSimpleAgreementAccountData g) = providedBalanceByAgreement g

    viewAccountTBA = tbaAccountData
    setAccountTBA acc aad t = acc { tbaAccountData = aad, accountLastUpdatedAt = t }
    viewAccountCFA = cfaAccountData
    setAccountCFA acc aad t = acc { cfaAccountData = aad, accountLastUpdatedAt = t }
    viewAccountDFA = dfaAccountData
    setAccountDFA acc aad t = acc { dfaAccountData = aad, accountLastUpdatedAt = t }

showAccountAt :: SimpleAccount -> SimpleTimestamp -> String
showAccountAt acc t =
    "Account @" ++ show(SF.addressOfAccount acc) ++
    "\n  Balance: " ++ show(SF.balanceOfAccountAt acc t) ++
    concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.agreementsOfAccount acc) ++
    "\n  Last Update: " ++ show(accountLastUpdatedAt acc)
    where agreementTypeTag (MkSimpleAgreementAccountData g) = tagFromValue g

create_simple_account :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
create_simple_account toAddress t = SimpleAccount
    { address = toAddress
    , tbaAccountData = def
    , cfaAccountData = def
    , dfaAccountData = def
    , accountLastUpdatedAt = t
    }

-- ============================================================================
-- | SimpleSystemData Type
--
newtype SimpleSystemData = SimpleSystemData
    { currentTime   :: SimpleTimestamp
    }

-- ============================================================================
-- | SimpleTokenData Type
--
data SimpleTokenData = SimpleTokenData
    { accounts           :: M.Map SimpleAddress SimpleAccount
    , cfaContractData    :: M.Map String (CFA.CFAContractData SimpleSuperfluidDistribution)
    , dfaContractData    :: M.Map String (DFA.DFAContractData SimpleSuperfluidDistribution)
    , tokenLastUpdatedAt :: SimpleTimestamp
    }
instance Default SimpleTokenData where
    def = SimpleTokenData
        { accounts = M.fromList [(minter_address, create_simple_account minter_address t)]
        , cfaContractData = def
        , dfaContractData = def
        , tokenLastUpdatedAt = t }
        where t = def :: SimpleTimestamp

minter_address :: SimpleAddress
minter_address = "_minter"

flow_acd_key :: SimpleAddress -> SimpleAddress -> String
flow_acd_key a b = (show a) ++ (show b)

-- ============================================================================
-- | Simple Monad Transformer stack
newtype SimpleSystemStateT m a = SimpleSystemStateT (ReaderT SimpleSystemData m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)
newtype SimpleTokenStateT m a = SimpleTokenStateT
    ( StateT SimpleTokenData
    ( SimpleSystemStateT m )
    a )
    deriving newtype (Functor, Applicative, Monad, MonadIO)
instance MonadTrans SimpleTokenStateT where
    lift m = SimpleTokenStateT $ StateT $ \s -> lift m >>= \a -> return (a, s)

getSystemData :: (Monad m) => SimpleTokenStateT m SimpleSystemData
getSystemData = SimpleTokenStateT . lift . SimpleSystemStateT $ ask

getSimpleTokenData :: (Monad m) => SimpleTokenStateT m SimpleTokenData
getSimpleTokenData = SimpleTokenStateT get

runSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m (a, SimpleTokenData)
runSimpleTokenStateT (SimpleTokenStateT m) sys token = m'' where
        (SimpleSystemStateT m') = runStateT m token
        m'' = runReaderT m' sys

evalSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m a
evalSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> fst

execSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m SimpleTokenData
execSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> snd

-- | SimpleTokenStateT State Internal Operations
--
_putSimpleTokenData :: (Monad m) => SimpleTokenData -> SimpleTokenStateT m ()
_putSimpleTokenData = SimpleTokenStateT . put

modify_token_data :: (Monad m) => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m ()
modify_token_data = SimpleTokenStateT . modify

-- | SimpleTokenStateT m is a SuperfluidToken instance
--
instance (Monad m) => SF.Token (SimpleTokenStateT m) where
    type TK_SFT (SimpleTokenStateT m) = SimpleSuperfluidDistribution

    type TK_ACC (SimpleTokenStateT m) = SimpleAccount

    getCurrentTime = getSystemData <&> currentTime

    getAccount addr = getSimpleTokenData >>= \s -> return $
        fromMaybe (create_simple_account addr 0) $ M.lookup addr (accounts s)

    putAccount addr acc = modify_token_data (\vs -> vs { accounts = M.insert addr acc (accounts vs) })

    getMinterAddress = return minter_address

    calcFlowBuffer = return  . (* Wad 3600)
    viewFlow a b = getSimpleTokenData >>= \s -> return $ M.lookup (flow_acd_key a b) (cfaContractData s)
    setFlow a b acd t = modify_token_data (
        \vs -> vs
               { cfaContractData = M.insert (flow_acd_key a b) acd (cfaContractData vs)
               , tokenLastUpdatedAt = t
               }
        )

    viewDecayingFlow a b = getSimpleTokenData >>= \s -> return $ M.lookup (flow_acd_key a b) (dfaContractData s)
    setDecayingFlow a b acd t = modify_token_data (
        \vs -> vs
               { dfaContractData = M.insert (flow_acd_key a b) acd (dfaContractData vs)
               , tokenLastUpdatedAt = t
               }
        )

-- | Other SimpleTokenStateT Operations
--
initSimpleToken :: (Monad m) => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    _putSimpleTokenData SimpleTokenData
        { accounts = M.fromList $ map (\a -> (a, create_simple_account a t)) alist
        , cfaContractData = M.empty
        , dfaContractData = M.empty
        , tokenLastUpdatedAt = t
        }
    mapM_ (`SF.mintLiquidity` initBalance) alist

addAccount :: (Monad m) => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accountAddr account = modify_token_data (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: (Monad m) => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = getSimpleTokenData <&> M.toList . accounts
