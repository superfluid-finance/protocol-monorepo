{-# OPTIONS_GHC -Wno-orphans #-}

module Money.Systems.Superfluid.Instances.Simple.Integrations.Serialization
  ( SimpleSerialized
  )
where

import qualified Data.Binary                                                      as B (Binary (..))
import qualified Data.Binary.Get                                                  as B (Get, runGet)
import qualified Data.Binary.Put                                                  as B (PutM, runPut)
import           Data.ByteString.Lazy                                             (ByteString)
import           Data.Coerce                                                      (coerce)

import           Money.Systems.Superfluid.Concepts.Liquidity
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import           Money.Systems.Superfluid.Instances.Simple.SuperfluidTypes

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
instance Serializable (TBA.TBAAccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (TBA.settledAt s)
        putLQ (coerce $ TBA.untappedLiquidity s)
        putLQ (coerce $ TBA.mintedLiquidity s)
    getter _ = do
        t <- getTS
        l <- getLQ
        m <- getLQ
        return TBA.TBAAccountData
            { TBA.settledAt = t
            , TBA.untappedLiquidity = UntappedLiquidity l
            , TBA.mintedLiquidity = TBA.mkMintedLiquidity m
            }

-- CFA
instance Serializable (CFA.CFAContractData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
    putter s = do
        putTS (CFA.flowLastUpdatedAt s)
        putLQ (coerce $ CFA.flowBuffer s)
        putLQ (CFA.flowRate s)
    getter _ = do
        t <- getTS
        b <- getLQ
        fr <- getLQ
        return CFA.CFAContractData
            { CFA.flowLastUpdatedAt = t
            , CFA.flowRate = fr
            , CFA.flowBuffer = BBS.mkBufferLiquidity b
            }

instance Serializable (CFA.CFAAccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
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
        return CFA.CFAAccountData
            { CFA.settledAt = t
            , CFA.settledUntappedLiquidity = UntappedLiquidity l
            , CFA.settledBufferLiquidity = BBS.mkBufferLiquidity b
            , CFA.netFlowRate = fr
            }

-- DFA
instance Serializable (DFA.DFAContractData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
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
        return DFA.DFAContractData
            { DFA.flowLastUpdatedAt = t
            , DFA.decayingFactor = λ
            , DFA.distributionLimit = θ
            , DFA.flowBuffer = BBS.mkBufferLiquidity b
            }

instance Serializable (DFA.DFAAccountData SimpleSuperfluidTypes) SimpleSuperfluidTypes where
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
        return DFA.DFAAccountData
            { DFA.settledAt = t
            ,  DFA.αVal = α
            , DFA.εVal = ε
            , DFA.settledBuffer = BBS.mkBufferLiquidity b
            }
