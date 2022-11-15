{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE MonoLocalBinds     #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop (tests) where

import           Data.Default
import           Data.Proxy
import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex                              as PDIDX
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                                         as CFMUD
import           Money.Systems.Superfluid.SystemTypes
import           Money.Systems.Superfluid.TestTypes

-- * Helpers

sameAs :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
(CFMUD.MkMonetaryUnitData a) `sameAs` (CFMUD.MkMonetaryUnitData b) =
    a^.CFMUD.settledValue == b^.CFMUD.settledValue &&
    a^.CFMUD.netFlowRate  == b^.CFMUD.netFlowRate

-- * Semigroup Monetary Unit Data Laws

semigroup_pubmud_associativity :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> Bool
semigroup_pubmud_associativity a b c = ((a <> b) <> c) `sameAs` (a <> (b <> c))

semigroup_pubmud_settles_pi :: T_CFDAPublisherMUD -> T_CFDAPublisherMUD -> T_Timestamp -> Bool
    = mud_prop_semigroup_settles_pi

-- * Agreement Laws

data TestOperations = Nop ()
                    | PubOp1 T_CFDAPublisherOperation
                    | PubOp2 T_CFDAPublisherOperation
                    | SubOp1 T_PDIDXSubscriberOperation
                    | SubOp2 T_PDIDXSubscriberOperation
                    deriving Show

newtype TO_1Pub2Subs = TO_1Pub2Subs TestOperations deriving Show
instance Arbitrary TO_1Pub2Subs where
    arbitrary = oneof [ (arbitrary :: Gen ()) <&> TO_1Pub2Subs . Nop
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO_1Pub2Subs . PubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO_1Pub2Subs . SubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO_1Pub2Subs . SubOp2
                      ]
ao_1pub2subs_zero_sum_balance :: T_Timestamp -> NonEmptyList (TO_1Pub2Subs, T_Timestamp) -> Bool
ao_1pub2subs_zero_sum_balance t0 aos0 = go (getNonEmpty aos0) t0 (def, def) def def
    where go ((TO_1Pub2Subs (Nop ()), tΔ):aos) t (dcFull, pubMuds) scFull1 scFull2 =
              let (isGood, (_, _)) = single_op
                      t' (dcFull, pubMuds) scFull1 scFull2
                      def NullAgreementOperation
              in  isGood && go aos t' (dcFull, pubMuds) scFull1 scFull2
              where t' = t + tΔ
          go ((TO_1Pub2Subs (PubOp1 ao), tΔ):aos) t (dcFull, pubMuds) scFull1 scFull2 =
              let cfdaDCFull = (PDIDX.dc_base dcFull, PDIDX.dc_cfda dcFull)
                  (isGood, ((_, cfdaDC'), CFDA.PublisherOperationOutputF pubMuds')) = single_op
                      t' (dcFull, pubMuds) scFull1 scFull2
                      (cfdaDCFull, CFDA.PublisherOperationOutputF pubMuds) ao
                  dcFull' = dcFull { PDIDX.dc_cfda = cfdaDC' }
              in  isGood && go aos t' (dcFull', pubMuds') scFull1 scFull2
              where t' = t + tΔ
          go ((TO_1Pub2Subs (PubOp2 _), _) :_) _ _ _ _ = error "huh? there is no pub2"
          go ((TO_1Pub2Subs (SubOp1 ao), tΔ):aos) t (dcFull, pubMuds) scFull1 scFull2 =
              let (isGood, ((dcFull', scFull1'), PDIDX.SubscriberOperationOutput pubMudsΔ)) = single_op
                      t' (dcFull, pubMuds) scFull1 scFull2
                      ((dcFull, scFull1), def) ao
              in  isGood && go aos t' (dcFull', pubMuds <> pubMudsΔ) scFull1' scFull2
              where t' = t + tΔ
          go ((TO_1Pub2Subs (SubOp2 ao), tΔ):aos) t (dcFull, pubMuds) scFull1 scFull2 =
              let (isGood, ((dcFull', scFull2'), PDIDX.SubscriberOperationOutput pubMudsΔ)) = single_op
                      t' (dcFull, pubMuds) scFull1 scFull2
                      ((dcFull, scFull2), def) ao
              in  isGood && go aos t' (dcFull', pubMuds <> pubMudsΔ) scFull1 scFull2'
              where t' = t + tΔ
          go [] _ _ _ _ = True
          single_op t' (dcFull, pubMuds) scFull1 scFull2 cs ao =
              let cfdaDCFull = (PDIDX.dc_base dcFull, PDIDX.dc_cfda dcFull)
                  ocss = [ MkAnyAgreementContractState (cfdaDCFull, CFDA.PublisherOperationOutputF pubMuds)
                         , MkAnyAgreementContractState ((dcFull, scFull1), def)
                         , MkAnyAgreementContractState ((dcFull, scFull2), def)
                         ]
              in  ao_go_zero_sum_balance_after_single_op
                  (Proxy @T_AnySemigroupMonetaryUnitData) fuzzyEqMVal
                  cs ao ocss t'

newtype TO_2Pubs1Sub = TO_2Pubs1Sub TestOperations deriving Show
instance Arbitrary TO_2Pubs1Sub where
    arbitrary = oneof [ (arbitrary :: Gen ()) <&> TO_2Pubs1Sub . Nop
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO_2Pubs1Sub . PubOp1
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO_2Pubs1Sub . PubOp2
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO_2Pubs1Sub . SubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO_2Pubs1Sub . SubOp2
                      ]
ao_2pubs1sub_zero_sum_balance :: T_Timestamp -> NonEmptyList (TO_2Pubs1Sub, T_Timestamp) -> Bool
ao_2pubs1sub_zero_sum_balance t0 aos0 = go (getNonEmpty aos0) t0 (def, def) (def, def) def def
    -- scFull1 is subscription between pub1 and sub1
    -- scFull2 is subscription between pub2 and sub1
    where go ((TO_2Pubs1Sub (Nop ()), tΔ):aos) t (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 =
              let (isGood, (_, _)) = single_op
                      t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
                      def NullAgreementOperation
              in  isGood && go aos t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
              where t' = t + tΔ
          go ((TO_2Pubs1Sub (PubOp1 ao), tΔ):aos) t (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 =
              let cfdaDCFull1 = (PDIDX.dc_base dcFull1, PDIDX.dc_cfda dcFull1)
                  (isGood, ((_, cfdaDC1'), CFDA.PublisherOperationOutputF pubMuds1')) = single_op
                      t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
                      (cfdaDCFull1, CFDA.PublisherOperationOutputF pubMuds1) ao
                  dcFull1' = dcFull1 { PDIDX.dc_cfda = cfdaDC1' }
              in  isGood && go aos t' (dcFull1', pubMuds1') (dcFull2, pubMuds2) scFull1 scFull2
              where t' = t + tΔ
          go ((TO_2Pubs1Sub (PubOp2 ao), tΔ):aos) t (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 =
              let cfdaDCFull2 = (PDIDX.dc_base dcFull2, PDIDX.dc_cfda dcFull2)
                  (isGood, ((_, cfdaDC2'), CFDA.PublisherOperationOutputF pubMuds2')) = single_op
                      t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
                      (cfdaDCFull2, CFDA.PublisherOperationOutputF pubMuds2) ao
                  dcFull2' = dcFull2 { PDIDX.dc_cfda = cfdaDC2' }
              in  isGood && go aos t' (dcFull1, pubMuds1) (dcFull2', pubMuds2') scFull1 scFull2
              where t' = t + tΔ
          go ((TO_2Pubs1Sub (SubOp1 ao), tΔ):aos) t (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 =
              let (isGood, ((dcFull1', scFull1'), PDIDX.SubscriberOperationOutput pubMuds1Δ)) = single_op
                      t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
                      ((dcFull1, scFull1), def) ao
              in  isGood && go aos t' (dcFull1', pubMuds1 <> pubMuds1Δ) (dcFull2, pubMuds2) scFull1' scFull2
              where t' = t + tΔ
          go ((TO_2Pubs1Sub (SubOp2 ao), tΔ):aos) t (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 =
              let (isGood, ((dcFull2', scFull2'), PDIDX.SubscriberOperationOutput pubMuds2Δ)) = single_op
                      t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2
                      ((dcFull2, scFull2), def) ao
              in  isGood && go aos t' (dcFull1, pubMuds1) (dcFull2', pubMuds2 <> pubMuds2Δ) scFull1 scFull2'
              where t' = t + tΔ
          go [] _ _ _ _ _ = True
          single_op t' (dcFull1, pubMuds1) (dcFull2, pubMuds2) scFull1 scFull2 cs ao =
              let cfdaDCFull1 = (PDIDX.dc_base dcFull1, PDIDX.dc_cfda dcFull1)
                  cfdaDCFull2 = (PDIDX.dc_base dcFull2, PDIDX.dc_cfda dcFull2)
                  ocss = [ MkAnyAgreementContractState (cfdaDCFull1, CFDA.PublisherOperationOutputF pubMuds1)
                         , MkAnyAgreementContractState (cfdaDCFull2, CFDA.PublisherOperationOutputF pubMuds2)
                         , MkAnyAgreementContractState ((dcFull1, scFull1), def)
                         , MkAnyAgreementContractState ((dcFull2, scFull2), def)
                         ]
              in  ao_go_zero_sum_balance_after_single_op
                  (Proxy @T_AnySemigroupMonetaryUnitData) fuzzyEqMVal
                  cs ao ocss t'

tests = describe "ConstantFlowDistributionAgreement properties" $ do
    it "CFDA semigroup Publisher MUD associativity"                       $ property semigroup_pubmud_associativity
    it "CFDA semigroup Publisher MUD settles pi"                          $ property semigroup_pubmud_settles_pi
    it "CFDA operations produces zero balance sum between 1 pub & 2 subs" $ property ao_1pub2subs_zero_sum_balance
    it "CFDA operations produces zero balance sum between 2 pubs & 1 sub" $ property ao_2pubs1sub_zero_sum_balance
