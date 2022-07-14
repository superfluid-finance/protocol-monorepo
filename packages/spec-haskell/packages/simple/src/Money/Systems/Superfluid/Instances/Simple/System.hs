{-# LANGUAGE DerivingVia       #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Money.Systems.Superfluid.Instances.Simple.System
    ( module Money.Systems.Superfluid.Instances.Simple.Types
    -- SimpleAccount
    , SimpleAddress
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
    , listAccounts
    , addAccount
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Binary                                                      (Binary)
import           Data.Char                                                        (isAlpha)
import           Data.Coerce                                                      (coerce)
import           Data.Default
import           Data.Functor
import qualified Data.Map                                                         as M
import           Data.Maybe
import           Data.String
import           Data.Type.TaggedTypeable
import           Lens.Micro

import qualified Money.Systems.Superfluid.Token                                   as SF
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
--
import qualified Money.Systems.Superfluid.Indexes.UniversalIndexes                as UIDX

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
    { address              :: SimpleAddress
    , itaMonetaryUnitData  :: UIDX.ITAMonetaryUnitData SimpleSuperfluidTypes
    , cfaMonetaryUnitData  :: UIDX.CFAMonetaryUnitData SimpleSuperfluidTypes
    , dfaMonetaryUnitData  :: UIDX.DFAMonetaryUnitData SimpleSuperfluidTypes
    , accountLastUpdatedAt :: SimpleTimestamp
    }

instance SF.MonetaryUnit SimpleAccount SimpleSuperfluidTypes where
    type AnyAgreementMonetaryUnitData SimpleAccount = AnySimpleAgreementMonetaryUnitData
    agreementsOf acc = [ MkSimpleAgreementMonetaryUnitData (acc^.SF.itaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.cfaMonetaryUnitData)
                       , MkSimpleAgreementMonetaryUnitData (acc^.SF.dfaMonetaryUnitData)
                       ]
    providedBalanceByAnyAgreement _ (MkSimpleAgreementMonetaryUnitData g) = balanceProvidedByAgreement g
    itaMonetaryUnitData   = lens itaMonetaryUnitData (\acc mud' -> acc { itaMonetaryUnitData = mud' })
    itaMonetaryUnitLens = to $ \acc -> let ITA.MkMonetaryUnitData l = itaMonetaryUnitData acc in l
    cfaMonetaryUnitData   = lens cfaMonetaryUnitData (\acc mud' -> acc { cfaMonetaryUnitData = mud' })
    cfaMonetaryUnitLens = to $ \acc -> let CFA.MkMonetaryUnitData l = cfaMonetaryUnitData acc in l
    dfaMonetaryUnitData   = lens dfaMonetaryUnitData (\acc mud' -> acc { dfaMonetaryUnitData = mud' })
    dfaMonetaryUnitLens = to $ \acc -> let DFA.MkMonetaryUnitData l = dfaMonetaryUnitData acc in l

instance SF.Account SimpleAccount SimpleSuperfluidTypes where
    type ACC_ADDR SimpleAccount = SimpleAddress
    addressOfAccount = address

showAccountAt :: SimpleAccount -> SimpleTimestamp -> String
showAccountAt acc t =
    "Account @" ++ show(SF.addressOfAccount acc) ++
    "\n  Balance: " ++ show(SF.balanceOfAt acc t) ++
    concatMap (\a -> "\n  " ++ agreementTypeTag a ++ ": " ++ show a) (SF.agreementsOf acc) ++
    "\n  Last Update: " ++ show(accountLastUpdatedAt acc)
    where agreementTypeTag (MkSimpleAgreementMonetaryUnitData g) = tagFromValue g

create_simple_account :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
create_simple_account toAddress t = SimpleAccount
    { address = toAddress
    , itaMonetaryUnitData = mempty
    , cfaMonetaryUnitData = mempty
    , dfaMonetaryUnitData = mempty
    , accountLastUpdatedAt = t
    }

-- | Simple system data type.
newtype SimpleSystemData = SimpleSystemData
    { currentTime   :: SimpleTimestamp
    }

-- | Simple token data type.
data SimpleTokenData = SimpleTokenData
    { accounts           :: M.Map SimpleAddress SimpleAccount
    , cfaContractData    :: M.Map String (UIDX.CFAContractData SimpleSuperfluidTypes)
    , dfaContractData    :: M.Map String (UIDX.DFAContractData SimpleSuperfluidTypes)
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

acd_key :: AgreementContractData acd amud SimpleSuperfluidTypes => (AgreementContractPartiesF acd) SimpleAddress -> String
acd_key = foldr ((++) . (++ ".") . coerce) def

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

-- | SimpleTokenStateT State Internal Operations.
--
put_simple_token_data :: (Monad m) => SimpleTokenData -> SimpleTokenStateT m ()
put_simple_token_data = SimpleTokenStateT . put

modify_token_data :: (Monad m) => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m ()
modify_token_data = SimpleTokenStateT . modify

-- | SimpleTokenStateT m is a @SF.Token@ instance.
instance (Monad m) => SF.Token (SimpleTokenStateT m) where

    type TK_SFT (SimpleTokenStateT m) = SimpleSuperfluidTypes
    type TK_ACC (SimpleTokenStateT m) = SimpleAccount

    getCurrentTime = getSystemData <&> currentTime

    getAccount addr = getSimpleTokenData >>= \s -> return $
        fromMaybe (create_simple_account addr 0) $ M.lookup addr (accounts s)

    putAccount addr acc = modify_token_data (\vs -> vs { accounts = M.insert addr acc (accounts vs) })

    -- * ITA
    --
    getMinterAddress = return minter_address
    viewITAContract _ = return $ Just def
    setITAContract  _ _ _ = return ()

    -- * CFA
    --
    calcFlowBuffer = return  . (* Wad 3600)
    viewFlow acps = getSimpleTokenData >>= \s -> return $ M.lookup (acd_key acps) (cfaContractData s)
    setFlow acps acd t = modify_token_data (
        \vs -> vs
               { cfaContractData = M.insert (acd_key acps) acd (cfaContractData vs)
               , tokenLastUpdatedAt = t
               }
        )

    -- * DFA
    --
    viewDecayingFlow acps = getSimpleTokenData >>= \s -> return $ M.lookup (acd_key acps) (dfaContractData s)
    setDecayingFlow acps acd t = modify_token_data (
        \vs -> vs
               { dfaContractData = M.insert (acd_key acps) acd (dfaContractData vs)
               , tokenLastUpdatedAt = t
               }
        )

-- ============================================================================
-- Other SimpleTokenStateT Operations
--
initSimpleToken :: (Monad m) => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    put_simple_token_data SimpleTokenData
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
