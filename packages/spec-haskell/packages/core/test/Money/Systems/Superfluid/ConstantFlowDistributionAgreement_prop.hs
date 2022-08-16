{-# OPTIONS_GHC -Wno-missing-signatures #-}

module Money.Systems.Superfluid.ConstantFlowDistributionAgreement_prop (tests) where

import           Data.Default
import           Data.Proxy
import           Lens.Internal
import           Test.Hspec
import           Test.QuickCheck

import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement     as CFDA
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.MonetaryUnitData.ConstantFlow                    as CFMUD
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

newtype TO1Pub2Subs = TO1Pub2Subs TestOperations deriving Show
instance Arbitrary TO1Pub2Subs where
    arbitrary = oneof [ (arbitrary :: Gen ()) <&> TO1Pub2Subs . Nop
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO1Pub2Subs . PubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO1Pub2Subs . SubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO1Pub2Subs . SubOp2
                      ]
ao_1pub2subs_zero_sum_balance :: T_Timestamp -> NonEmptyList (TO1Pub2Subs, T_Timestamp) -> Bool
ao_1pub2subs_zero_sum_balance t0 aos0 = go (getNonEmpty aos0) t0 (def, def) def def
    where go ((TO1Pub2Subs (Nop ()), tΔ):aos) t (dcFull, muds) scFull1 scFull2 =
              go' aos t' (dcFull, muds) scFull1 scFull2
              where t' = t + tΔ
          go ((TO1Pub2Subs (PubOp1 ao), tΔ):aos) t (dcFull, muds) scFull1 scFull2 =
              let cfdaDCFull = (PDIDX.dc_base dcFull, PDIDX.dc_cfda dcFull)
                  ((_, cfdaDC'), CFDA.PublisherOperationOutputF muds') =
                      ao_go_single_op (cfdaDCFull, CFDA.PublisherOperationOutputF muds) ao t'
                  dcFull' = dcFull { PDIDX.dc_cfda = cfdaDC' }
              in  go' aos t' (dcFull', muds') scFull1 scFull2
              where t' = t + tΔ
          go ((TO1Pub2Subs (PubOp2 _), _) :_) _ _ _ _ = error "huh"
          go ((TO1Pub2Subs (SubOp1 ao), tΔ):aos) t (dcFull, muds) scFull1 scFull2 =
              let ((_, scFull1'), mudsΔ') =
                      ao_go_single_op ((dcFull, scFull1), def) ao t'
              in  go' aos t' (dcFull, muds <> mudsΔ') scFull1' scFull2
              where t' = t + tΔ
          go ((TO1Pub2Subs (SubOp2 ao), tΔ):aos) t (dcFull, muds) scFull1 scFull2 =
              let ((_, scFull2'), mudsΔ') =
                      ao_go_single_op ((dcFull, scFull2), def) ao t'
              in  go' aos t' (dcFull, muds <> mudsΔ') scFull1 scFull2'
              where t' = t + tΔ
          go [] _ _ _ _ = True
          go' aos' t' (dcFull', muds') sc1Full' sc2Full' =
              is_zero_sum t' (dcFull', muds') sc1Full' sc2Full'
              && go aos' t' (dcFull', muds') sc1Full' sc2Full'
          is_zero_sum t' (dcFull', muds') sc1Full' sc2Full' =
              let cfdaDCFull' = (PDIDX.dc_base dcFull', PDIDX.dc_cfda dcFull')
              in foldMap (flip (ao_sum_contract_balance p) t')
                 [ MkAnyAgreementContractState (cfdaDCFull', CFDA.PublisherOperationOutputF muds')
                 , MkAnyAgreementContractState ((dcFull', sc1Full'), def)
                 , MkAnyAgreementContractState ((dcFull', sc2Full'), def)
                 ] == mempty
              where p = Proxy @T_AnySemigroupMonetaryUnitData

newtype TO2Pubs1Sub = TO2Pubs1Sub TestOperations deriving Show
instance Arbitrary TO2Pubs1Sub where
    arbitrary = oneof [ (arbitrary :: Gen ()) <&> TO2Pubs1Sub . Nop
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO2Pubs1Sub . PubOp1
                      , (arbitrary :: Gen T_CFDAPublisherOperation) <&> TO2Pubs1Sub . PubOp2
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO2Pubs1Sub . SubOp1
                      , (arbitrary :: Gen T_PDIDXSubscriberOperation) <&> TO2Pubs1Sub . SubOp2
                      ]
ao_2pubs1sub_zero_sum_balance :: T_Timestamp -> NonEmptyList (TO2Pubs1Sub, T_Timestamp) -> Bool
ao_2pubs1sub_zero_sum_balance t0 aos0 = go (getNonEmpty aos0) t0 (def, def) (def, def) def
    where
          go ((TO2Pubs1Sub (Nop ()), tΔ):aos) t (dcFull1, muds1) (dcFull2, muds2) scFull =
              go' aos t' (dcFull1, muds1) (dcFull2, muds2) scFull
              where t' = t + tΔ
          go ((TO2Pubs1Sub (PubOp1 ao), tΔ):aos) t (dcFull1, muds1) (dcFull2, muds2) scFull =
              let cfdaDCFull1 = (PDIDX.dc_base dcFull1, PDIDX.dc_cfda dcFull1)
                  ((_, cfdaDC1'), CFDA.PublisherOperationOutputF muds1') =
                      ao_go_single_op (cfdaDCFull1, CFDA.PublisherOperationOutputF muds1) ao t'
                  dcFull1' = dcFull1 { PDIDX.dc_cfda = cfdaDC1' }
              in  go' aos t' (dcFull1', muds1') (dcFull2, muds2) scFull
              where t' = t + tΔ
          go ((TO2Pubs1Sub (PubOp2 ao), tΔ):aos) t (dcFull1, muds1) (dcFull2, muds2) scFull =
              let cfdaDCFull2 = (PDIDX.dc_base dcFull2, PDIDX.dc_cfda dcFull2)
                  ((_, cfdaDC2'), CFDA.PublisherOperationOutputF muds2') =
                      ao_go_single_op (cfdaDCFull2, CFDA.PublisherOperationOutputF muds2) ao t'
                  dcFull1' = dcFull1 { PDIDX.dc_cfda = cfdaDC2' }
              in  go' aos t' (dcFull1', muds1) (dcFull2, muds2') scFull
              where t' = t + tΔ
          go ((TO2Pubs1Sub (SubOp1 ao), tΔ):aos) t (dcFull1, muds1) (dcFull2, muds2) scFull =
              let ((_, scFull'), muds1Δ') =
                      ao_go_single_op ((dcFull1, scFull), def) ao t'
              in  go' aos t' (dcFull1, muds1 <> muds1Δ') (dcFull2, muds2) scFull'
              where t' = t + tΔ
          go ((TO2Pubs1Sub (SubOp2 ao), tΔ):aos) t (dcFull1, muds1) (dcFull2, muds2) scFull =
              let ((_, scFull'), muds2Δ') =
                      ao_go_single_op ((dcFull2, scFull), def) ao t'
              in  go' aos t' (dcFull1, muds1) (dcFull2, muds2 <> muds2Δ') scFull'
              where t' = t + tΔ
          go [] _ _ _ _ = True
          go' aos' t' (dcFull', muds') sc1Full' sc2Full' =
              is_zero_sum t' (dcFull', muds') sc1Full' sc2Full'
              && go aos' t' (dcFull', muds') sc1Full' sc2Full'
          is_zero_sum t' (dcFull1', muds1')  (dcFull2', muds2') scFull' =
              let cfdaDCFull1' = (PDIDX.dc_base dcFull1', PDIDX.dc_cfda dcFull1')
                  cfdaDCFull2' = (PDIDX.dc_base dcFull2', PDIDX.dc_cfda dcFull2')
              in foldMap (flip (ao_sum_contract_balance p) t')
                 [ MkAnyAgreementContractState (cfdaDCFull1', CFDA.PublisherOperationOutputF muds1')
                 , MkAnyAgreementContractState (cfdaDCFull2', CFDA.PublisherOperationOutputF muds2')
                 , MkAnyAgreementContractState ((dcFull1', scFull'), def)
                 , MkAnyAgreementContractState ((dcFull2', scFull'), def)
                 ] == mempty
              where p = Proxy @T_AnySemigroupMonetaryUnitData

tests = describe "ConstantFlowDistributionAgreement properties" $ do
    it "CFDA semigroup Publisher MUD associativity"                       $ property semigroup_pubmud_associativity
    it "CFDA semigroup Publisher MUD settles pi"                          $ property semigroup_pubmud_settles_pi
    it "CFDA operations produces zero balance sum between 1 pub & 2 subs" $ property ao_1pub2subs_zero_sum_balance
    it "CFDA operations produces zero balance sum between 2 pubs & 1 sub" $ property ao_2pubs1sub_zero_sum_balance
