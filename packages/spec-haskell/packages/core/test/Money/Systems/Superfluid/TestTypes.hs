{-# LANGUAGE DerivingVia #-}

module Money.Systems.Superfluid.TestTypes where

import           Control.Applicative
import           Data.Default
import           Data.Typeable
import           GHC.Generics

import           Test.QuickCheck

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.MonetaryUnitData.MintedValue as MVMUD
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS


-- * Value
newtype TestMVal = TestMVal Integer
    deriving (Default, Eq, Enum, Real, Ord, Num, Integral, Value, Show)

instance Arbitrary TestMVal where
    arbitrary = TestMVal <$> arbitrary

-- * RealTimeBalance
data TestRealTimeBalanceF a = TestRealTimeBalanceF
    { untappedValue :: a
    , mintedValue   :: a
    , depositValue  :: a
    }
    deriving stock (Generic, Functor, Foldable, Traversable, Show)
type TestRealTimeBalance = TestRealTimeBalanceF TestMVal

instance Applicative TestRealTimeBalanceF where
    pure a = TestRealTimeBalanceF a a a
    liftA2 f (TestRealTimeBalanceF a b c) (TestRealTimeBalanceF a' b' c') =
        TestRealTimeBalanceF (f a a') (f b b') (f c c')
instance Semigroup (TestRealTimeBalanceF TestMVal) where (<>) = liftA2 (+)
instance Monoid (TestRealTimeBalanceF TestMVal) where mempty = pure 0

instance RealTimeBalance TestRealTimeBalanceF TestMVal where
    valueToRTB uval = TestRealTimeBalanceF uval def def

    typedValuesToRTB (UntappedValue uval) tvec =
        TestRealTimeBalanceF uval def def <> foldMap g tvec
        -- extra correctly typed RTB monoid
        where g (AnyTappedValue (p, v)) = case typeRep p of
                  t | t == typeRep MVMUD.mintedValueTag -> TestRealTimeBalanceF def   v def
                    | t == typeRep BBS.bufferValueTag  -> TestRealTimeBalanceF def def   v
                    | otherwise -> error "Invalid monetary value tag"

    typedValuesFromRTB rtb = (UntappedValue (untappedValue rtb),
                              [ mkAnyTappedValue $ MVMUD.mkMintedValue $ mintedValue rtb
                              , mkAnyTappedValue $ BBS.mkBufferValue   $ depositValue rtb
                              ])

instance Arbitrary TestRealTimeBalance where
    arbitrary = do
        u  <- arbitrary
        m  <- arbitrary
        d  <- arbitrary
        return TestRealTimeBalanceF
            { untappedValue    = TestMVal u
            , mintedValue      = TestMVal m
            , depositValue     = TestMVal d
            }
