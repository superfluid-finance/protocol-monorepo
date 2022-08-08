{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Token
    ( module Money.Systems.Superfluid.MonetaryUnit
    , Address
    , Account (..)
    , ProportionalDistributionIndexID
    , Token (..)
    ) where

import           Data.Default
import           Data.Foldable                                                             (toList)
import           Data.Kind                                                                 (Type)
import           Data.Maybe                                                                (fromMaybe)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency                   as BBS
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement                 as CFA
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowDistributionAgreement     as CFDA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement                 as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantDistributionAgreement          as IDA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement              as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement                       as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
--
import           Money.Systems.Superfluid.MonetaryUnit

-- | Address type class.
--
-- Notional conventions:
--  * Type name: addr
class Eq addr => Address addr

-- | MonetaryUnit type class.
--
-- Notional conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *MonetaryUnit
class (SuperfluidTypes sft, MonetaryUnit acc sft) => Account acc sft | acc -> sft where
    type ACC_ADDR acc :: Type

-- | ID for the proportional distribution index. Maybe an indexed type instead?
type ProportionalDistributionIndexID = Int

-- | Token Type Class
--
-- Notional conventions:
--   * Type name: tk
--   * Term name: *Token
--
-- Notes:
--
-- * Token is a monadic type, where all its functions run within the monadic context.
-- * Token provides:
--   * addressable account,
--   * and agreement (ITA/CFA/GDA) operations.
-- * Instructions for write operations are executed in `execTokenInstructions`.
class ( Monad tk
      , SuperfluidTypes sft
      , Account acc sft
      ) => Token tk acc sft | tk -> acc, tk -> sft where
    -- * System Functions
    --

    getCurrentTime :: tk (SFT_TS sft)

    -- * Account Functions
    --

    getAccount :: ACC_ADDR acc -> tk acc

    putAccount :: ACC_ADDR acc -> acc -> SFT_TS sft -> tk ()

    balanceOfAccount :: ACC_ADDR acc -> tk (SFT_RTB sft)
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAt account t

    -- * Agreements operations over the universal index
    --

    -- TODO make this even more polymorphic to work with proportional distribution index
    updateUniversalIndex
        :: ( AgreementOperation ao sft
           , acd ~ AgreementOperationData ao     -- this is a useful property of universal-indexed agreement operations
           , amud ~ AgreementMonetaryUnitDataInOperation ao
           , Default (AgreementOperationData ao)
           , Traversable (AgreementOperationResultF ao)
           )
        => (AgreementOperationResultF ao) (ACC_ADDR acc)                                  -- aorAddrs
        -> ao                                                                             -- ao
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> tk (Maybe acd))              -- acdGetter
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> acd -> SFT_TS sft -> tk ())  -- acdSetter
        -> Lens' acc amud                                                                 -- amuLs
        -> tk ()
    updateUniversalIndex aorAddrs ao acdGetter acdSetter amuData = do
        -- load acd and accounts data
        t <- getCurrentTime
        aod <- fromMaybe def <$> acdGetter aorAddrs
        aorAccounts <- mapM getAccount aorAddrs
        -- apply agreement operation
        let (acd', aorΔamuds) = applyAgreementOperation ao aod t
        -- append delta to existing amuds
        let amuds' = zipWith (<>) (fmap (^. amuData) (toList aorAccounts)) (toList aorΔamuds)
        -- set new acd
        acdSetter aorAddrs acd' t
        -- set new amuds
        mapM_ (\(addr, amud) -> putAccount addr amud t)
            (zip (toList aorAddrs)
                (fmap (uncurry (set amuData))
                    (zip amuds' (toList aorAccounts))))

    -- ** Minter Functions
    --

    getMinterAddress :: tk (ACC_ADDR acc)

    viewMinterContract :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> tk (Maybe (MINTA.ContractData sft))
    setMinterContract  :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> MINTA.ContractData sft -> SFT_TS sft -> tk ()

    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = do
        minterAddress <- getMinterAddress
        updateUniversalIndex
            (MINTA.OperationResultF minterAddress toAddr) (MINTA.Mint amount)
            viewMinterContract setMinterContract minterMonetaryUnitData

    -- ** ITA Functions
    --

    viewITAContract :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> tk (Maybe (ITA.ContractData sft))
    setITAContract  :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> ITA.ContractData sft -> SFT_TS sft -> tk ()

    transfer :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> SFT_MVAL sft -> tk ()
    transfer aorAddrs amount = do
        updateUniversalIndex
            aorAddrs (ITA.Transfer amount)
            viewITAContract setITAContract itaMonetaryUnitData

    -- ** CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    viewFlow :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> tk (Maybe (CFA.ContractData sft))
    setFlow  :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> CFA.ContractData sft -> SFT_TS sft -> tk ()

    updateFlow :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> CFA.FlowRate sft -> tk ()
    updateFlow aorAddrs newFlowRate = do
        newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        updateUniversalIndex
            aorAddrs (CFA.UpdateFlow newFlowRate newFlowBuffer)
            viewFlow setFlow cfaMonetaryUnitData

    -- ** DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> tk (Maybe (DFA.ContractData sft))
    setDecayingFlow  :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> DFA.ContractData sft -> SFT_TS sft -> tk ()

    updateDecayingFlow :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> DFA.DistributionLimit sft -> tk ()
    updateDecayingFlow aorAddrs newDistributionLimit = do
        updateUniversalIndex
            aorAddrs (DFA.UpdateDecayingFlow newDistributionLimit def)
            viewDecayingFlow setDecayingFlow dfaMonetaryUnitData

    -- * Agreements operations over proportional distribution indexes
    --

    -- ** IDA Functions
    --

    viewProportionalDistributionContract
        :: ACC_ADDR acc                                -- publisher
        -> ProportionalDistributionIndexID             -- indexId
        -> tk (Maybe (PDIDX.DistributionContract sft))

    setProportionalDistributionContract
        :: ACC_ADDR acc                                -- publisher
        -> ProportionalDistributionIndexID             -- indexId
        -> PDIDX.DistributionContract sft              -- index
        -> SFT_TS sft                                  -- t
        -> tk ()

    viewProportionalDistributionSubscription
        :: ACC_ADDR acc                                -- subscriber
        -> ACC_ADDR acc                                -- publisher
        -> ProportionalDistributionIndexID             -- indexId
        -> tk (Maybe (PDIDX.SubscriptionContract sft))

    setProportionalDistributionSubscription
        :: ACC_ADDR acc                                -- subscriber
        -> ACC_ADDR acc                                -- publisher
        -> ProportionalDistributionIndexID             -- indexId
        -> PDIDX.SubscriptionContract sft              -- sub
        -> SFT_TS sft                                  -- t
        -> tk ()

    updateProportionalDistributionSubscription
        :: ACC_ADDR acc                    -- subscriber
        -> ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_FLOAT sft                   -- unit
        -> tk ()
    updateProportionalDistributionSubscription subscriber publisher indexId unit = do
        -- load acd and accounts data
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- fromMaybe def <$> viewProportionalDistributionContract publisher indexId
        sc  <- fromMaybe def <$> viewProportionalDistributionSubscription subscriber publisher indexId
        let (dc1, sc1) = settle_ida t (dc, sc)
        let (dc2, sc2, cfdaMUDΔ) = settle_cfda t (dc1, sc1)
        let (dc', sc') = subscribe_unit t (dc2, sc2)
        setProportionalDistributionContract publisher indexId dc' t
        setProportionalDistributionSubscription subscriber publisher indexId sc' t
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> cfdaMUDΔ) pub) t
        where
            settle_ida t (dc, sc) = let
                aod = (PDIDX.dc_base dc, PDIDX.dc_ida dc, PDIDX.sc_base sc, PDIDX.sc_ida sc)
                aod' = applyAgreementOperation IDA.SettleSubscription (IDA.SubscriberOperationData aod) t
                (IDA.SubscriberOperationData (_, dc_ida', _, sc_ida'), _) = aod'
                in (dc { PDIDX.dc_ida = dc_ida' }, sc { PDIDX.sc_ida = sc_ida' })
            settle_cfda t (dc, sc) = let
                aod = (PDIDX.dc_base dc, PDIDX.dc_cfda dc, PDIDX.sc_base sc, PDIDX.sc_cfda sc)
                aod' = applyAgreementOperation CFDA.SettleSubscription (CFDA.SubscriberOperationData aod) t
                ( CFDA.SubscriberOperationData (_, dc_cfda', _, sc_cfda')
                    , CFDA.SubscriberOperationPartiesF cfdaMUDΔ ) = aod'
                in (dc { PDIDX.dc_cfda = dc_cfda' }, sc { PDIDX.sc_cfda = sc_cfda' }, cfdaMUDΔ)
            subscribe_unit t (dc, sc) = let
                aod = (dc, sc)
                aod' = applyAgreementOperation (PDIDX.Subscribe unit) (PDIDX.SubscriberOperationData aod) t
                (PDIDX.SubscriberOperationData (dc', sc'), _) = aod'
                in (dc', sc')

    distributeProportionally
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- amount
        -> tk ()
    distributeProportionally publisher indexId amount = do
        -- load acd and accounts data
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- fromMaybe def <$> viewProportionalDistributionContract publisher indexId
        let (IDA.PublisherOperationData _ dc_ida', IDA.PublisherOperationResultF amudΔ) =
                applyAgreementOperation
                (IDA.Distribute amount)
                (IDA.PublisherOperationData (PDIDX.dc_base dc) (PDIDX.dc_ida dc))
                t
        setProportionalDistributionContract publisher indexId (dc { PDIDX.dc_ida = dc_ida' }) t
        putAccount publisher (over idaPublisherMonetaryUnitData (<> amudΔ) pub) t

    distributeFlow
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- flowRate
        -> tk ()
    distributeFlow publisher indexId flowRate = do
        -- load acd and accounts data
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- fromMaybe def <$> viewProportionalDistributionContract publisher indexId
        let (CFDA.PublisherOperationData _ dc_cfda', CFDA.PublisherOperationResultF amudΔ) =
                applyAgreementOperation
                (CFDA.UpdateDistributionFlowRate flowRate)
                (CFDA.PublisherOperationData (PDIDX.dc_base dc) (PDIDX.dc_cfda dc))
                t
        setProportionalDistributionContract publisher indexId (dc { PDIDX.dc_cfda = dc_cfda' }) t
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> amudΔ) pub) t

-- ============================================================================
-- Internal
--
type CONTRACT_ACC_ADDR acc ao = AgreementOperationResultF ao (ACC_ADDR acc)
