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
           , mud ~ MonetaryUnitDataInOperation ao
           , Default acd
           , Traversable (AgreementOperationResultF ao)
           )
        => (AgreementOperationResultF ao) (ACC_ADDR acc)                                  -- aorAddrs
        -> ao                                                                             -- ao
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> tk acd)                      -- acdGetter
        -> ((AgreementOperationResultF ao) (ACC_ADDR acc) -> acd -> SFT_TS sft -> tk ())  -- acdSetter
        -> Lens' acc mud                                                                 -- amuLs
        -> tk ()
    effAgreementOperation aorAddrs ao acdGetter acdSetter amuData = do
        -- load acd and accounts data
        t <- getCurrentTime
        acd <- acdGetter aorAddrs
        aorAccounts <- mapM getAccount aorAddrs
        -- apply agreement operation
        let (acd', aorΔmuds) = applyAgreementOperation ao acd t
        -- append delta to existing muds
        let muds' = zipWith (<>) (fmap (^. amuData) (toList aorAccounts)) (toList aorΔmuds)
        -- set new acd
        acdSetter aorAddrs acd' t
        -- set new muds
        mapM_ (\(addr, mud) -> putAccount addr mud t)
            (zip (toList aorAddrs)
                (fmap (uncurry (set amuData))
                    (zip muds' (toList aorAccounts))))

    -- ** Minter Functions
    --

    getMinterAddress :: tk (ACC_ADDR acc)

    view_minter_contract :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> tk (MINTA.ContractData sft)
    set_minter_contract  :: CONTRACT_ACC_ADDR acc (MINTA.Operation sft) -> MINTA.ContractData sft -> SFT_TS sft -> tk ()

    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = getMinterAddress >>= \minterAddress -> effAgreementOperation
        (MINTA.OperationResultF minterAddress toAddr)
        (MINTA.Mint amount)
        view_minter_contract set_minter_contract
        minterMonetaryUnitData

    -- ** ITA Functions
    --

    view_ita_contract :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> tk (ITA.ContractData sft)
    set_ita_contract  :: CONTRACT_ACC_ADDR acc (ITA.Operation sft) -> ITA.ContractData sft -> SFT_TS sft -> tk ()

    transfer :: ACC_ADDR acc -> ACC_ADDR acc -> SFT_MVAL sft -> tk ()
    transfer fromAddr toAddr amount = effAgreementOperation
        (ITA.OperationResultF fromAddr toAddr)
        (ITA.Transfer amount)
        view_ita_contract set_ita_contract
        itaMonetaryUnitData

    -- ** CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    view_flow :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> tk (CFA.ContractData sft)
    set_flow  :: CONTRACT_ACC_ADDR acc (CFA.Operation sft) -> CFA.ContractData sft -> SFT_TS sft -> tk ()

    getFlow :: ACC_ADDR acc -> ACC_ADDR acc ->  tk (CFA.ContractData sft)
    getFlow sender receiver = view_flow (CFA.OperationResultF sender receiver)
    updateFlow :: ACC_ADDR acc -> ACC_ADDR acc -> CFA.FlowRate sft -> tk ()
    updateFlow sender receiver newFlowRate = effAgreementOperation
        (CFA.OperationResultF sender receiver)
        (CFA.UpdateFlow newFlowRate)
        view_flow set_flow
        cfaMonetaryUnitData
        -- TODO newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate

    -- ** DFA Functions
    --

    view_decaying_flow :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> tk (DFA.ContractData sft)
    set_decaying_flow  :: CONTRACT_ACC_ADDR acc (DFA.Operation sft) -> DFA.ContractData sft -> SFT_TS sft -> tk ()

    getDecayingFlow :: ACC_ADDR acc -> ACC_ADDR acc ->  tk (DFA.ContractData sft)
    getDecayingFlow sender receiver = view_decaying_flow (DFA.OperationResultF sender receiver)
    updateDecayingFlow :: ACC_ADDR acc -> ACC_ADDR acc -> DFA.DistributionLimit sft -> tk ()
    updateDecayingFlow sender receiver newDistributionLimit = effAgreementOperation
        (DFA.OperationResultF sender receiver)
        (DFA.UpdateDecayingFlow newDistributionLimit def)
        view_decaying_flow set_decaying_flow
        dfaMonetaryUnitData

    -- ** IDA Functions
    --

    viewProportionalDistributionContract
        :: ACC_ADDR acc                                                       -- publisher
        -> ProportionalDistributionIndexID                                    -- indexId
        -> tk (PDIDX.DistributionContract sft)
    overProportionalDistributionContract
        :: ACC_ADDR acc                                                       -- publisher
        -> ProportionalDistributionIndexID                                    -- indexId
        -> (PDIDX.DistributionContract sft -> PDIDX.DistributionContract sft) -- updater
        -> SFT_TS sft                                                         -- t
        -> tk ()

    viewProportionalDistributionSubscription
        :: ACC_ADDR acc                                                       -- subscriber
        -> ACC_ADDR acc                                                       -- publisher
        -> ProportionalDistributionIndexID                                    -- indexId
        -> tk (PDIDX.SubscriptionContract sft)
    overProportionalDistributionSubscription
        :: ACC_ADDR acc                                                       -- subscriber
        -> ACC_ADDR acc                                                       -- publisher
        -> ProportionalDistributionIndexID                                    -- indexId
        -> (PDIDX.SubscriptionContract sft -> PDIDX.SubscriptionContract sft) -- updater
        -> SFT_TS sft                                                         -- t
        -> tk ()

    updateProportionalDistributionSubscription
        :: ACC_ADDR acc                    -- subscriber
        -> ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_FLOAT sft                   -- unit
        -> tk ()
    updateProportionalDistributionSubscription subscriber publisher indexId unit = do
        -- TODO refactor and use effAgreementOperation
        t <- getCurrentTime
        pub <- getAccount publisher
        dc <- viewProportionalDistributionContract publisher indexId
        sc  <- viewProportionalDistributionSubscription subscriber publisher indexId
        let ((dc', sc'), cfdaMUDΔ) = PDIDX.updateSubscription (dc, sc) unit t
        overProportionalDistributionContract publisher indexId (const dc') t
        overProportionalDistributionSubscription subscriber publisher indexId (const sc') t
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> cfdaMUDΔ) pub) t

    distributeProportionally
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- amount
        -> tk ()
    distributeProportionally publisher indexId amount = effAgreementOperation
        (IDA.PublisherOperationResultF publisher)
        (IDA.Distribute amount)
        viewContract setContract
        idaPublisherMonetaryUnitData
        where viewContract (IDA.PublisherOperationResultF addr) =
                  viewProportionalDistributionContract addr indexId >>= \dc ->
                  return (IDA.PublisherContract (PDIDX.dc_base dc) (PDIDX.dc_ida dc))
              setContract (IDA.PublisherOperationResultF addr) (IDA.PublisherContract _ dc_ida') t =
                  overProportionalDistributionContract addr indexId (\dc -> dc { PDIDX.dc_ida = dc_ida' }) t

    distributeFlow
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- flowRate
        -> tk ()
    distributeFlow publisher indexId flowRate = effAgreementOperation
        (CFDA.PublisherOperationResultF publisher)
        (CFDA.UpdateDistributionFlowRate flowRate)
        viewContract setContract
        cfdaPublisherMonetaryUnitData
        where viewContract (CFDA.PublisherOperationResultF addr) =
                  viewProportionalDistributionContract addr indexId >>= \dc ->
                  return (CFDA.PublisherContract (PDIDX.dc_base dc) (PDIDX.dc_cfda dc))
              setContract (CFDA.PublisherOperationResultF addr) (CFDA.PublisherContract _ dc_cfda') t =
                  overProportionalDistributionContract addr indexId (\dc -> dc { PDIDX.dc_cfda = dc_cfda' }) t

-- ============================================================================
-- Internal
--
type CONTRACT_ACC_ADDR acc ao = AgreementOperationResultF ao (ACC_ADDR acc)
