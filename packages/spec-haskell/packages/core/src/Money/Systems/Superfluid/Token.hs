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
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
-- import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency                   as BBS
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

    -- * Agreements operations
    --

    -- | Effectuate an agreement operation application
    effAgreementOperation
        :: ( AgreementOperation ao sft
           , acd ~ AgreementContract ao     -- this is a useful property of universal-indexed agreement operations
           , amud ~ AgreementMonetaryUnitDataInOperation ao
           , Default acd
           , Traversable (AgreementOperationResultF ao)
           )
        => (AgreementOperationResultF ao) (ACC_ADDR acc)                                  -- aorAddrs
        -> ao                                                                             -- ao
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> tk acd)                      -- acdGetter
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> acd -> SFT_TS sft -> tk ())  -- acdSetter
        -> Lens' acc amud                                                                 -- amuLs
        -> tk ()
    effAgreementOperation aorAddrs ao acdGetter acdSetter amuData = do
        -- load acd and accounts data
        t <- getCurrentTime
        acd <- acdGetter aorAddrs
        aorAccounts <- mapM getAccount aorAddrs
        -- apply agreement operation
        let (acd', aorΔamuds) = applyAgreementOperation ao acd t
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

    viewMinterContract :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> tk (MINTA.ContractData sft)
    setMinterContract  :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> MINTA.ContractData sft -> SFT_TS sft -> tk ()
    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = do
        minterAddress <- getMinterAddress
        effAgreementOperation
            (MINTA.OperationResultF minterAddress toAddr) (MINTA.Mint amount)
            viewMinterContract setMinterContract
            minterMonetaryUnitData

    -- ** ITA Functions
    --

    viewITAContract :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> tk (ITA.ContractData sft)
    setITAContract  :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> ITA.ContractData sft -> SFT_TS sft -> tk ()
    transfer :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> SFT_MVAL sft -> tk ()
    transfer aorAddrs amount = do
        effAgreementOperation
            aorAddrs (ITA.Transfer amount)
            viewITAContract setITAContract
            itaMonetaryUnitData

    -- ** CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    viewFlow :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> tk (CFA.ContractData sft)
    setFlow  :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> CFA.ContractData sft -> SFT_TS sft -> tk ()
    updateFlow :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> CFA.FlowRate sft -> tk ()
    updateFlow aorAddrs newFlowRate = do
        -- newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        effAgreementOperation
            aorAddrs (CFA.UpdateFlow newFlowRate)
            viewFlow setFlow
            cfaMonetaryUnitData

    -- ** DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> tk (DFA.ContractData sft)
    setDecayingFlow  :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> DFA.ContractData sft -> SFT_TS sft -> tk ()
    updateDecayingFlow :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> DFA.DistributionLimit sft -> tk ()
    updateDecayingFlow aorAddrs newDistributionLimit = do
        effAgreementOperation
            aorAddrs (DFA.UpdateDecayingFlow newDistributionLimit def)
            viewDecayingFlow setDecayingFlow dfaMonetaryUnitData

    -- ** IDA Functions
    --

    viewProportionalDistributionContract
        :: ACC_ADDR acc                                -- publisher
        -> ProportionalDistributionIndexID             -- indexId
        -> tk (PDIDX.DistributionContract sft)
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
        -> tk (PDIDX.SubscriptionContract sft)
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
        dc <- viewProportionalDistributionContract publisher indexId
        sc  <- viewProportionalDistributionSubscription subscriber publisher indexId
        let ((dc', sc'), cfdaMUDΔ) = PDIDX.updateSubscription (dc, sc) unit t
        setProportionalDistributionContract publisher indexId dc' t
        setProportionalDistributionSubscription subscriber publisher indexId sc' t
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> cfdaMUDΔ) pub) t

    distributeProportionally
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- amount
        -> tk ()
    distributeProportionally publisher indexId amount = do
        -- load acd and accounts data
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- viewProportionalDistributionContract publisher indexId
        let (IDA.PublisherContract _ dc_ida', IDA.PublisherOperationResultF amudΔ) =
                applyAgreementOperation
                (IDA.Distribute amount)
                (IDA.PublisherContract (PDIDX.dc_base dc) (PDIDX.dc_ida dc))
                t
        setProportionalDistributionContract publisher indexId (dc { PDIDX.dc_ida = dc_ida' }) t
        putAccount publisher (over idaPublisherMonetaryUnitData (<> amudΔ) pub) t

    distributeFlow
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- flowRate
        -> tk ()
    distributeFlow publisher indexId flowRate = do
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- viewProportionalDistributionContract publisher indexId
        let (CFDA.PublisherContract _ dc_cfda', CFDA.PublisherOperationResultF amudΔ) =
                applyAgreementOperation
                (CFDA.UpdateDistributionFlowRate flowRate)
                (CFDA.PublisherContract (PDIDX.dc_base dc) (PDIDX.dc_cfda dc))
                t
        setProportionalDistributionContract publisher indexId (dc { PDIDX.dc_cfda = dc_cfda' }) t
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> amudΔ) pub) t
        -- effAgreementOperation
        --     aorAddrs (CFDA.UpdateDistributionFlowRate flowRate)
        --     viewContract setContract
        --     cfdaPublisherMonetaryUnitData
        --     where aorAddrs = (CFDA.PublisherOperationResultF) publisher
        --           viewContract (CFDA.PublisherOperationResultF addr) =
        --               viewProportionalDistributionContract addr indexId >>= \dc ->
        --               return (CFDA.PublisherContract (PDIDX.dc_base dc) (PDIDX.dc_cfda dc))
        --           setContract (CFDA.PublisherOperationResultF addr) (CFDA.PublisherContract dc_base' dc_cfda') _ =
        --               setProportionalDistributionContract addr indexId dc'

-- ============================================================================
-- Internal
--
type CONTRACT_ACC_ADDR acc ao = AgreementOperationResultF ao (ACC_ADDR acc)
