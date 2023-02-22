{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Theory.SemanticMoney_prop (tests) where

import           Data.Default
import           Test.Hspec
import           Test.QuickCheck

import           Money.Theory.SemanticMoney
import           Money.Theory.TestMonetaryTypes


--------------------------------------------------------------------------------
-- Monoidal laws for universal index
--------------------------------------------------------------------------------

uidx_monoid_identity a = a == a <> (mempty :: TestUniversalIndex) &&
                         a == (mempty :: TestUniversalIndex) <> a

uidx_monoid_assoc a b c = ((a :: TestUniversalIndex) <> b) <> c == a <> (b <> c)

uidx_monoid_laws = describe "uidx monoidal laws" $ do
    it "uidx monoid identity law" $ property uidx_monoid_identity
    it "uidx monoid associativity law" $ property uidx_monoid_assoc

--------------------------------------------------------------------------------
-- 1to1 2-primitives
--------------------------------------------------------------------------------

uidx_uidx_f1_f2 f1 f2 t1 {- f1 -} t2 {- f2 -} t3 =
    0 == rtb a' t3 + rtb b' t3
    where (a, b) = (def :: TestUniversalIndex, def :: TestUniversalIndex)
          (a', b') = f2 t2 (f1 t1 (a, b))

uidx_uidx_twice_shift2 x1 x2 = uidx_uidx_f1_f2 (shift2 x1) (shift2 x2)
uidx_uidx_twice_flow2 r1 r2 = uidx_uidx_f1_f2 (flow2 r1) (flow2 r2)
uidx_uidx_shift2_flow2 x r = uidx_uidx_f1_f2 (shift2 x) (flow2 r)
uidx_uidx_flow2_shift2 r x = uidx_uidx_f1_f2 (flow2 r) (shift2 x)

one2one_tests = describe "1to1 2-primitives" $ do
        it "uidx:uidx twice shift2" $ property uidx_uidx_twice_shift2
        it "uidx:uidx twice flow2"  $ property uidx_uidx_twice_flow2
        it "uidx:uidx shift2 flow2" $ property uidx_uidx_shift2_flow2
        it "uidx:uidx flow2 shift2" $ property uidx_uidx_flow2_shift2

--------------------------------------------------------------------------------
-- 1toN proportional distribution 2-primitives
--------------------------------------------------------------------------------

uidx_pdidx_u1_f1_u1_f2 f1 f2 t1 u1 t2 {- f1 -} t3 {- f2 -} t4 u2 t5 =
    0 == rtb a'' t5 + rtb (b'', b1') t5
    where (a, (b, b1)) = pdpUpdateMember2 u1 t1 (def :: (TestUniversalIndex, TestPDPoolMemberMU))
          (a', b') = f2 t3 (f1 t2 (a, b))
          (a'', (b'', b1')) = pdpUpdateMember2 u2 t4 (a', (b', b1))

uidx_pdidx_u1_f1_u2_f2 f1 f2 t1 u1 t2 {- f1 -} t3 u2 t4 {- f2 -} t5 =
    0 == rtb a''' t5 + rtb (b''', b1) t5 + rtb (b''', b2) t5
    where (a, (b, b1)) = pdpUpdateMember2 u1 t1 (def :: TestUniversalIndex, def :: TestPDPoolMemberMU)
          (a', b') = f1 t2 (a, b)
          (a'', (b'', b2)) = pdpUpdateMember2 u2 t3 (a', (b', def :: TestPDPoolMember))
          (a''', b''') = f2 t4 (a'', b'')

uidx_pdidx_u1_shift2_u1_shift2 x1 x2 = uidx_pdidx_u1_f1_u1_f2 (shift2 x1) (shift2 x2)
uidx_pdidx_u1_flow2_u1_flow2 r1 r2 = uidx_pdidx_u1_f1_u1_f2 (flow2 r1) (flow2 r2)
uidx_pdidx_u1_shift2_u1_flow2 x r = uidx_pdidx_u1_f1_u1_f2 (shift2 x) (flow2 r)
uidx_pdidx_u1_flow2_u1_shift2 r x = uidx_pdidx_u1_f1_u1_f2 (shift2 r) (flow2 x)
uidx_pdidx_u1_shift2_u2_shift2 x1 x2 = uidx_pdidx_u1_f1_u2_f2 (shift2 x1) (shift2 x2)
uidx_pdidx_u1_flow2_u2_flow2 r1 r2 = uidx_pdidx_u1_f1_u2_f2 (flow2 r1) (flow2 r2)
uidx_pdidx_u1_flow2_u2_shift2 r x = uidx_pdidx_u1_f1_u2_f2 (flow2 r) (shift2 x)
uidx_pdidx_u1_shift2_u2_flow2 x r = uidx_pdidx_u1_f1_u2_f2 (shift2 x) (flow2 r)

one2n_pd_tests = describe "1toN proportional distribution 2-primitives" $ do
    it "uidx:pdidx u1 shift2 u1 shift2" $ property uidx_pdidx_u1_shift2_u1_shift2
    it "uidx:pdidx u1 flow2  u1 flow2"  $ property uidx_pdidx_u1_flow2_u1_flow2
    it "uidx:pdidx u1 shift2 u1 flow2"  $ property uidx_pdidx_u1_shift2_u1_flow2
    it "uidx:pdidx u1 flow2  u1 shift2" $ property uidx_pdidx_u1_flow2_u1_shift2
    it "uidx:pdidx u1 shift2 u2 shift2"  $ property uidx_pdidx_u1_shift2_u2_shift2
    it "uidx:pdidx u1 flow2  u2 flow2"  $ property uidx_pdidx_u1_flow2_u2_flow2
    it "uidx:pdidx u1 flow2  u2 shift2" $ property uidx_pdidx_u1_flow2_u2_shift2
    it "uidx:pdidx u1 shift2 u2 flow2"  $ property uidx_pdidx_u1_shift2_u2_flow2

--------------------------------------------------------------------------------
-- (Constant Rate) Flow 2-Primitive
--------------------------------------------------------------------------------

uidx_uidx_flow2 t1 r1 t2 r2 t3 =
    r1 `mt_v_mul_t` (t2 - t1) + r2 `mt_v_mul_t` (t3 - t2) == rtb b' t3
    where (a, b) = (def :: TestUniversalIndex, def :: TestUniversalIndex)
          (_, b') = flow2 r2 t2 (flow2 r1 t1 (a, b))

uidx_pdidx_flow2 t1 r1 t2 r2 t3 =
    r1 `mt_v_mul_t` (t2 - t1) + r2 `mt_v_mul_t` (t3 - t2) == rtb (b', b1) t3
    where (a, (b, b1)) = pdpUpdateMember2 1 t1 (def :: (TestUniversalIndex, TestPDPoolMemberMU))
          (_, b') = flow2 r2 t2 (flow2 r1 t1 (a, b))

flow2_tests = describe "flow2 tests" $ do
    it "uidx:uidx flow2" $ property uidx_uidx_flow2
    it "uidx:pdidx flow2" $ property uidx_pdidx_flow2

tests = describe "Semantic money properties" $ do
    uidx_monoid_laws
    one2one_tests
    one2n_pd_tests
    flow2_tests
