{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE DerivingVia  #-}
{-# LANGUAGE TypeFamilies #-}

module Money.Systems.Superfluid.TestTypes where

import           Control.Applicative
import           Data.Coerce
import           Data.Default
import           Data.Typeable
import           GHC.Generics

import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement             as CFA
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow     as CFMUD
import qualified Money.Systems.Superfluid.MonetaryUnitData.MintedValue      as MVMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency               as BBS


-- * Timestamp

newtype TTTimestamp = TTTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp, Show, Arbitrary)

-- * Value
newtype TTMVal = TTMVal Integer
    deriving (Default, Eq, Enum, Real, Ord, Num, Integral, Value, Show, Arbitrary)

deriving instance Show (UntappedValue TTMVal)

-- * RealTimeBalance
data TTRealTimeBalanceF a = TTRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Show, Eq)
type TTRealTimeBalance = TTRealTimeBalanceF TTMVal

instance Applicative TTRealTimeBalanceF where
    pure a = TTRealTimeBalanceF a a a
    liftA2 f (TTRealTimeBalanceF a b c) (TTRealTimeBalanceF a' b' c') =
        TTRealTimeBalanceF (f a a') (f b b') (f c c')
instance Semigroup (TTRealTimeBalanceF TTMVal) where (<>) = liftA2 (+)
instance Monoid (TTRealTimeBalanceF TTMVal) where mempty = pure 0
-- instance Eq (TTRealTimeBalanceF TTMVal) where (==) = (==)

instance RealTimeBalance TTRealTimeBalanceF TTMVal where
    valueToRTB uval = TTRealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        TTRealTimeBalanceF uval def def <> foldMap g tvec
        -- extra correctly typed RTB monoid
        where g (AnyTappedValue (p, v)) = case typeRep p of
                  t | t == typeRep MVMUD.mintedValueTag -> TTRealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag  -> TTRealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MVMUD.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

instance Arbitrary TTRealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        return TTRealTimeBalanceF
            { untappedValue = TTMVal u
            , mintedValue   = TTMVal m
            , depositValue  = TTMVal d
            }

-- * SuperfluidTypes Type

data TTSuperfluidTypes

instance SFTFloat Double

instance SuperfluidTypes TTSuperfluidTypes where
    type SFT_FLOAT TTSuperfluidTypes = Double
    type SFT_MVAL  TTSuperfluidTypes = TTMVal
    type SFT_TS    TTSuperfluidTypes = TTTimestamp
    type SFT_RTB_F TTSuperfluidTypes = TTRealTimeBalanceF
    dfa_default_lambda _ = log 2 / (3600 * 24 * 7)

-- * BBS
deriving instance Show (BBS.BufferValue TTMVal)

-- * CFA

type TTCFAMUD = CFA.MonetaryUnitData TTSuperfluidTypes

instance Arbitrary TTCFAMUD where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        nfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFA.MonetaryUnitLenses
            { CFA.settled_at    = TTTimestamp t_s
            , CFA.settled_value = coerce $ TTMVal sv
            , CFA.net_flow_rate = TTMVal nfr
            }
deriving instance Show (CFA.MonetaryUnitLenses TTSuperfluidTypes)
deriving instance Show TTCFAMUD

-- * CFDA

type TTCFDAPublisherMUD = CFDA.PublisherMonetaryUnitData TTSuperfluidTypes

instance Arbitrary TTCFDAPublisherMUD where
    arbitrary = do
        t_s <- arbitrary
        sv  <- arbitrary
        tfr <- arbitrary
        return $ CFMUD.MkMonetaryUnitData CFDA.PublisherData
            { CFDA.pub_settled_at      = TTTimestamp t_s
            , CFDA.pub_settled_value   = coerce $ TTMVal sv
            , CFDA.pub_total_flow_rate = TTMVal tfr
            }
deriving instance Show (CFDA.PublisherData TTSuperfluidTypes)
deriving instance Show TTCFDAPublisherMUD
