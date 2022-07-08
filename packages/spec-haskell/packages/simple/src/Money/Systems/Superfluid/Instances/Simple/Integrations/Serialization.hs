{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Systems.Superfluid.Instances.Simple.Integrations.Serialization
  ( SimpleSerialized
  , Serialized (..)
  )
where

import qualified Data.Binary                                                      as B (Binary (..))
import qualified Data.Binary.Get                                                  as B (Get, runGet)
import qualified Data.Binary.Put                                                  as B (PutM, runPut)
import           Data.ByteString.Lazy                                             (ByteString)
import           Data.Coerce                                                      (coerce)
import           Data.Proxy


import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import           Money.Systems.Superfluid.Instances.Simple.Types


-- ============================================================================
-- Putter/Getter Serializable Framework
--

class (Monad srl, SuperfluidTypes sft) => Putter srl sft | srl -> sft where
    putFloat :: SFT_FLOAT sft -> srl ()
    putLQ :: SFT_LQ sft -> srl ()
    putTS :: SFT_TS sft -> srl ()

class (Monad srl, SuperfluidTypes sft) => Getter srl sft | srl -> sft where
    getFloat :: srl (SFT_FLOAT sft)
    getLQ :: srl (SFT_LQ sft)
    getTS :: srl (SFT_TS sft)

class SuperfluidTypes sft => Serializable a sft | a -> sft where
    getter :: Getter srl sft => Proxy a -> srl a
    putter :: Putter srl sft => a -> srl ()

class SuperfluidTypes sft => Serialized s sft | s -> sft where
    runGetter :: Serializable a sft => Proxy a -> s -> a
    runPutter :: Serializable a sft => a -> s

-- ============================================================================
-- Serializable For Simple SuperfluidTypes
--

newtype SimpleSerialized = SimpleSerialized ByteString
newtype SimpleGetter a = SimpleGetter (B.Get a) deriving (Functor, Applicative, Monad)
newtype SimplePutter a = SimplePutter (B.PutM a) deriving (Functor, Applicative, Monad)

instance Getter SimpleGetter SimpleSuperfluidTypes where
    getFloat = SFDouble <$> SimpleGetter B.get
    getLQ = SimpleGetter B.get
    getTS = SimpleGetter B.get

instance Putter SimplePutter SimpleSuperfluidTypes where
    putFloat x = SimplePutter . B.put $ (coerce x :: Double)
    putLQ = SimplePutter . B.put
    putTS = SimplePutter . B.put

instance Serialized SimpleSerialized SimpleSuperfluidTypes where
    runGetter taggedProxy (SimpleSerialized s) = B.runGet m s where (SimpleGetter m) = getter taggedProxy
    runPutter a = SimpleSerialized $ B.runPut m where (SimplePutter m) = putter a

-- ============================================================================
-- Known Serializable Instances
--

-- TBA
instance Serializable (TBA.AccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putLQ (coerce $ TBA.untappedLiquidity s)
        putLQ (coerce $ TBA.mintedLiquidity s)
    getter _ = do
        l <- getLQ
        m <- getLQ
        return TBA.AccountData
            { TBA.untappedLiquidity = UntappedValue l
            , TBA.mintedLiquidity = TBA.mkMintedLiquidity m
            }

-- CFA
instance Serializable (CFA.ContractData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (CFA.flowLastUpdatedAt s)
        putLQ (coerce $ CFA.flowBuffer s)
        putLQ (CFA.flowRate s)
    getter _ = do
        t <- getTS
        b <- getLQ
        fr <- getLQ
        return CFA.ContractData
            { CFA.flowLastUpdatedAt = t
            , CFA.flowRate = fr
            , CFA.flowBuffer = BBS.mkBufferLiquidity b
            }

instance Serializable (CFA.AccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (CFA.settledAt s)
        putLQ (coerce $ CFA.settledUntappedLiquidity s)
        putLQ (coerce $ CFA.settledBufferLiquidity s)
        putLQ (CFA.netFlowRate s)
    getter _ = do
        t <- getTS
        l <- getLQ
        b <- getLQ
        fr <- getLQ
        return CFA.AccountData
            { CFA.settledAt = t
            , CFA.settledUntappedLiquidity = UntappedValue l
            , CFA.settledBufferLiquidity = BBS.mkBufferLiquidity b
            , CFA.netFlowRate = fr
            }

-- DFA
instance Serializable (DFA.ContractData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (DFA.flowLastUpdatedAt s)
        putFloat (DFA.decayingFactor s)
        putLQ (DFA.distributionLimit s)
        putLQ (coerce $ DFA.flowBuffer s)
    getter _ = do
        t <- getTS
        λ <- getFloat
        θ <- getLQ
        b <- getLQ
        return DFA.ContractData
            { DFA.flowLastUpdatedAt = t
            , DFA.decayingFactor = λ
            , DFA.distributionLimit = θ
            , DFA.flowBuffer = BBS.mkBufferLiquidity b
            }

instance Serializable (DFA.AccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (DFA.settledAt s)
        putFloat (DFA.αVal s)
        putFloat (DFA.εVal s)
        putLQ (coerce $ DFA.settledBuffer s)
    getter _ = do
        t <- getTS
        α <- getFloat
        ε <- getFloat
        b <- getLQ
        return DFA.AccountData
            { DFA.settledAt = t
            ,  DFA.αVal = α
            , DFA.εVal = ε
            , DFA.settledBuffer = BBS.mkBufferLiquidity b
            }
