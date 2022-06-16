{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}

module Money.Superfluid.Instances.Simple.System
    ( module Money.Superfluid.Instances.Simple.SuperfluidTypes
    -- SimpleAccount
    , SimpleAccount
    , SF.Account (..)
    , SF.balanceOfAccountAt
    , SF.sumAccounts
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
import qualified Data.Map                                          as M
import           Data.Maybe

import           Money.Superfluid.Concepts.Agreement               (agreementTypeTag)
--
import           Data.Internal.TaggedTypeable
--
import qualified Money.Superfluid.System.AccountTokenModel         as SF
import qualified Money.Superfluid.System.Serialization             as S

import           Money.Superfluid.Instances.Simple.Serialization
import           Money.Superfluid.Instances.Simple.SuperfluidTypes

-- ============================================================================
-- SimpleAccount Type and Operations (is SuperfluidAccount)
--
data SimpleAccount = SimpleAccount
    { address              :: SimpleAddress
    , agreementAccountData :: M.Map String SimpleSerialized
    , accountLastUpdatedAt :: SimpleTimestamp
    }

instance SF.Account SimpleAccount SimpleSuperfluidTypes where
    addressOfAccount = address

    agreementOfAccount taggedProxy acc = S.runGetter taggedProxy
         <$> M.lookup k (agreementAccountData acc)
         where k = tagFromProxy taggedProxy

    updateAgreementOfAccount acc aad t = acc
        { agreementAccountData = M.insert k (S.runPutter aad) (agreementAccountData acc)
        , accountLastUpdatedAt = t
        }
        where k = tagFromType aad

    showAccountAt acc t =
        "Account @" ++ show(SF.addressOfAccount acc) ++
        "\n  Balance: " ++ show(SF.balanceOfAccountAt acc t) ++
        concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.agreementsOfAccount acc) ++
        "\n  Last Update: " ++ show(accountLastUpdatedAt acc)

_createSimpleAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
_createSimpleAccount toAddress t = SimpleAccount
    { address = toAddress
    , agreementAccountData = def
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
    { accounts              :: M.Map SimpleAddress SimpleAccount
    , agreementContractData :: M.Map String SimpleSerialized
    , tokenLastUpdatedAt    :: SimpleTimestamp
    }
instance Default SimpleTokenData where
    def = SimpleTokenData
        { accounts = M.fromList [(_minterAddress, _createSimpleAccount _minterAddress t)]
        , agreementContractData = def
        , tokenLastUpdatedAt = t }
        where t = def :: SimpleTimestamp

_minterAddress :: SimpleAddress
_minterAddress = "_minter"

_acdKey :: [SimpleAddress] -> String
_acdKey = concatMap (("." ++) . show)

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

_modifySimpleTokenData :: (Monad m) => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m ()
_modifySimpleTokenData = SimpleTokenStateT . modify

-- | SimpleTokenStateT m is a SuperfluidToken instance
--
instance (Monad m) => SF.Token (SimpleTokenStateT m) where

    type TK_SFT (SimpleTokenStateT m) = SimpleSuperfluidTypes

    type TK_ACC (SimpleTokenStateT m) = SimpleAccount

    getCurrentTime = getSystemData <&> currentTime

    getAgreementContractData taggedProxy addrs = getSimpleTokenData >>= \s -> return $
        S.runGetter taggedProxy <$> M.lookup (_acdKey addrs) (agreementContractData s)

    putAgreementContractData addrs t acd = _modifySimpleTokenData (\vs -> vs
        { agreementContractData = M.insert (_acdKey addrs) (S.runPutter acd) (agreementContractData vs)
        , tokenLastUpdatedAt = t
        })

    getAccount addr = getSimpleTokenData >>= \s -> return $
        fromMaybe (_createSimpleAccount addr 0) $ M.lookup addr (accounts s)

    putAccount addr acc = _modifySimpleTokenData (\vs -> vs { accounts = M.insert addr acc (accounts vs) })

    getMinterAddress = return _minterAddress

    calcFlowBuffer = return  . (* Wad 3600)


-- | Other SimpleTokenStateT Operations
--
initSimpleToken :: (Monad m) => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    _putSimpleTokenData SimpleTokenData
        { accounts = M.fromList $ map (\a -> (a, _createSimpleAccount a t)) alist
        , agreementContractData = M.empty
        , tokenLastUpdatedAt = t
        }
    mapM_ (`SF.mintLiquidity` initBalance) alist

addAccount :: (Monad m) => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accountAddr account = _modifySimpleTokenData (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: (Monad m) => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = getSimpleTokenData <&> M.toList . accounts
