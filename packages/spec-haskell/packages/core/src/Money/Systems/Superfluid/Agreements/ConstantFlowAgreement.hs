{-# LANGUAGE DerivingVia            #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Agreements.ConstantFlowAgreement
    ( MonetaryUnitLens (..)
    , MonetaryUnitData (..)
    , ContractLens (..)
    , ContractData (..)
    , AgreementContractPartiesF (..)
    , AgreementOperation (..)
    , ContractPartiesF
    , ContractPartiesMUD
    ) where

import           Control.Applicative                                     (Applicative (..))
import           Data.Coerce                                             (coerce)
import           Data.Default                                            (Default (..))
import           Data.Kind                                               (Type)
import           Data.Type.TaggedTypeable                                (TaggedTypeable (..))
import           Data.Typeable                                           (Typeable)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- FIXME use: newtype FlowRate sft = FlowRate (SFT_LQ sft)

-- * CFA.MonetaryUnitData
--

class (Typeable mud, Default mud, SuperfluidTypes sft) => MonetaryUnitLens mud sft | mud -> sft where
    settledAt                :: Lens' mud (SFT_TS sft)
    settledUntappedLiquidity :: Lens' mud (UntappedValue (SFT_LQ sft))
    settledBufferLiquidity   :: Lens' mud (BBS.BufferLiquidity (SFT_LQ sft))
    netFlowRate              :: Lens' mud (SFT_LQ sft)

type MonetaryUnitData :: Type -> Type -> Type -- kind signature is required to make GHC happy
newtype MonetaryUnitData _mud sft = MkMonetaryUnitData _mud
instance MonetaryUnitLens mud sft => TaggedTypeable (MonetaryUnitData mud sft) where
    tagFromProxy _ = "CFA"

instance MonetaryUnitLens _mud sft => Semigroup (MonetaryUnitData _mud sft) where
    (<>) (MkMonetaryUnitData a) (MkMonetaryUnitData b) =
        let c = a & set  settledAt                (  b^.settledAt)
                  & over settledUntappedLiquidity (+ b^.settledUntappedLiquidity)
                  & over netFlowRate              (+ b^.netFlowRate)
                  & over settledBufferLiquidity   (+ b^.settledBufferLiquidity)
        in MkMonetaryUnitData c
instance MonetaryUnitLens _mud sft => Monoid (MonetaryUnitData _mud sft) where mempty = MkMonetaryUnitData def

instance MonetaryUnitLens _mud sft => AgreementMonetaryUnitData (MonetaryUnitData _mud sft) sft where
    balanceProvidedByAgreement (MkMonetaryUnitData a) t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedValue $ uval_s + calc_value_delta fr t_s t )
            [ mkAnyTappedLiquidity buf_s ]
        where t_s                  = a^.settledAt
              UntappedValue uval_s = a^.settledUntappedLiquidity
              buf_s                = a^.settledBufferLiquidity
              fr                   = a^.netFlowRate

-- * TBA.ContractData
--

class (Typeable cd, Default cd, SuperfluidTypes sft) => ContractLens cd sft | cd -> sft where
    flowLastUpdatedAt :: Lens' cd (SFT_TS sft)
    flowRate          :: Lens' cd (SFT_LQ sft)
    flowBuffer        :: Lens' cd (BBS.BufferLiquidity (SFT_LQ sft))

type ContractData :: Type -> Type -> Type -> Type
newtype ContractData _cd mud sft = MkContractData _cd
instance (ContractLens _cd sft, Typeable mud) => TaggedTypeable (ContractData _cd mud sft) where
    tagFromProxy _ = "CFA#"
instance ContractLens _cd sft => Default (ContractData _cd mud sft) where def = MkContractData def

instance ( ContractLens _cd sft
         , MonetaryUnitLens _mud sft
         , AgreementMonetaryUnitData (MonetaryUnitData _mud sft) sft
         ) => AgreementContractData (ContractData _cd _mud sft) (MonetaryUnitData _mud sft) sft where

    data AgreementContractPartiesF (ContractData _cd _mud sft) a = ContractPartiesF
        { flowSender   :: a
        , flowReceiver :: a
        } deriving stock (Functor, Foldable, Traversable)

    data AgreementOperation (ContractData _cd _mud sft) =
        --         flowRate     newFlowBuffer                      t'
        UpdateFlow (SFT_LQ sft) (BBS.BufferLiquidity (SFT_LQ sft)) (SFT_TS sft)

    applyAgreementOperation (MkContractData acd) acps (UpdateFlow newFlowRate newFlowBuffer t') = let
        acd' = acd & set flowRate newFlowRate
                   & set flowBuffer newFlowBuffer
                   & set flowLastUpdatedAt t'
        acps' = (<>) <$> acps <*> fmap MkMonetaryUnitData (ContractPartiesF
                    (def & set settledAt t'
                         & set netFlowRate (- flowRateDelta)
                         & set settledUntappedLiquidity (UntappedValue $ (- flowPeriodDelta) - coerce flowBufferDelta)
                         & set settledBufferLiquidity flowBufferDelta)
                    (def & set settledAt t'
                         & set netFlowRate flowRateDelta
                         & set settledUntappedLiquidity (UntappedValue flowPeriodDelta)
                         & set settledBufferLiquidity def))
        in (MkContractData acd', acps')
        where
            t               = acd^.flowLastUpdatedAt
            fr              = acd^.flowRate
            flowPeriodDelta = calc_value_delta fr t t'
            flowRateDelta   = newFlowRate - fr
            flowBufferDelta = newFlowBuffer - acd^.flowBuffer

type ContractPartiesF   sft _cd mud = AgreementContractPartiesF (ContractData _cd mud sft)
type ContractPartiesMUD sft _cd mud = ContractPartiesF sft _cd (MonetaryUnitData mud sft)

instance Applicative (ContractPartiesF sft cd mud) where
    pure a = ContractPartiesF a a
    liftA2 f (ContractPartiesF s r) (ContractPartiesF s' r') = ContractPartiesF (f s s') (f r r')

-- ============================================================================
-- Internal functions
--
-- Calculate value delta for settlement
calc_value_delta :: (Value v, Timestamp ts) => v -> ts -> ts -> v
calc_value_delta fr t0 t1 = fr * fromIntegral (t1 - t0)
