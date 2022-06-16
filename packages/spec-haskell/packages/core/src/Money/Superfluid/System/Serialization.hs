{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Superfluid.System.Serialization
  ( Putter (..),
    Getter (..),
    Serializable (..),
    Serialized (..),
  )
where

import           Data.Proxy
--

--
import           Money.Superfluid.Concepts.Liquidity                      (UntappedLiquidity (..), untypeLiquidity)
import           Money.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes (..))
--
import           Data.Internal.TaggedTypeable                 (TaggedTypeable)
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency          as BBS

class (Monad srl, SuperfluidTypes sft) => Putter srl sft | srl -> sft where
    putFloat :: SFT_FLOAT sft -> srl ()
    putLQ :: SFT_LQ sft -> srl ()
    putTS :: SFT_TS sft -> srl ()

class (Monad srl, SuperfluidTypes sft) => Getter srl sft | srl -> sft where
    getFloat :: srl (SFT_FLOAT sft)
    getLQ :: srl (SFT_LQ sft)
    getTS :: srl (SFT_TS sft)

class (TaggedTypeable a, SuperfluidTypes sft) => Serializable a sft | a -> sft where
    putter :: Putter srl sft => a -> srl ()
    getter :: Getter srl sft => Proxy a -> srl a

class SuperfluidTypes sft => Serialized s sft | s -> sft where
    runGetter :: Serializable a sft => Proxy a -> s -> a
    runPutter :: Serializable a sft => a -> s

-- ============================================================================

-- | Known Serializable Instances
--
-- TBA
instance SuperfluidTypes sft => Serializable (TBA.TBAAccountData sft) sft where
  putter s = do
    putTS (TBA.settledAt s)
    putLQ (untypeLiquidity $ TBA.untappedLiquidity s)
    putLQ (untypeLiquidity $ TBA.mintedLiquidity s)
  getter _ = do
    t <- getTS
    l <- getLQ
    m <- getLQ
    return
      TBA.TBAAccountData
        { TBA.settledAt = t,
          TBA.untappedLiquidity = UntappedLiquidity l,
          TBA.mintedLiquidity = TBA.mkMintedLiquidity m
        }

-- CFA
instance SuperfluidTypes sft => Serializable (CFA.CFAContractData sft) sft where
  putter s = do
    putTS (CFA.flowLastUpdatedAt s)
    putLQ (untypeLiquidity $ CFA.flowBuffer s)
    putLQ (CFA.flowRate s)
  getter _ = do
    t <- getTS
    b <- getLQ
    fr <- getLQ
    return
      CFA.CFAContractData
        { CFA.flowLastUpdatedAt = t,
          CFA.flowRate = fr,
          CFA.flowBuffer = BBS.mkBufferLiquidity b
        }

instance SuperfluidTypes sft => Serializable (CFA.CFAAccountData sft) sft where
  putter s = do
    putTS (CFA.settledAt s)
    putLQ (untypeLiquidity $ CFA.settledUntappedLiquidity s)
    putLQ (untypeLiquidity $ CFA.settledBufferLiquidity s)
    putLQ (CFA.netFlowRate s)
  getter _ = do
    t <- getTS
    l <- getLQ
    b <- getLQ
    fr <- getLQ
    return
      CFA.CFAAccountData
        { CFA.settledAt = t,
          CFA.settledUntappedLiquidity = UntappedLiquidity l,
          CFA.settledBufferLiquidity = BBS.mkBufferLiquidity b,
          CFA.netFlowRate = fr
        }


-- DFA
instance SuperfluidTypes sft => Serializable (DFA.DFAContractData sft) sft where
    putter s = do
        putTS (DFA.flowLastUpdatedAt s)
        putFloat (DFA.decayingFactor s)
        putLQ (DFA.distributionLimit s)
        putLQ (untypeLiquidity $ DFA.flowBuffer s)
    getter _ = do
        t <- getTS
        λ <- getFloat
        θ <- getLQ
        b <- getLQ
        return
            DFA.DFAContractData
            { DFA.flowLastUpdatedAt = t,
              DFA.decayingFactor = λ,
              DFA.distributionLimit = θ,
              DFA.flowBuffer = BBS.mkBufferLiquidity b
            }

instance SuperfluidTypes sft => Serializable (DFA.DFAAccountData sft) sft where
  putter s = do
    putTS (DFA.settledAt s)
    putFloat (DFA.αVal s)
    putFloat (DFA.εVal s)
    putLQ (untypeLiquidity $ DFA.settledBuffer s)
  getter _ = do
    t <- getTS
    α <- getFloat
    ε <- getFloat
    b <- getLQ
    return
      DFA.DFAAccountData
        { DFA.settledAt = t,
          DFA.αVal = α,
          DFA.εVal = ε,
          DFA.settledBuffer = BBS.mkBufferLiquidity b
        }
