{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Token
    ( module Money.Systems.Superfluid.MonetaryUnit
    , Address
    , Account (..)
    , ProportionalDistributionIndexID
    , Token (..)
    ) where

import           Data.Foldable                                                                                  (toList)
import           Data.Kind                                                                                      (Type)
import           Lens.Internal

import           Money.Systems.Superfluid.SystemTypes
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.ConstantFlowDistributionAgreement as CFDA
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistribution.InstantDistributionAgreement      as IDA
import qualified Money.Systems.Superfluid.Agreements.Universal.ConstantFlowAgreement                            as CFA
import qualified Money.Systems.Superfluid.Agreements.Universal.DecayingFlowAgreement                            as DFA
import qualified Money.Systems.Superfluid.Agreements.Universal.InstantTransferAgreement                         as ITA
import qualified Money.Systems.Superfluid.Agreements.Universal.MinterAgreement                                  as MINTA
--
import qualified Money.Systems.Superfluid.Agreements.ProportionalDistributionIndex                              as PDIDX
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
class (SuperfluidSystemTypes sft, MonetaryUnit acc sft) => Account acc sft | acc -> sft where
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
      , SuperfluidSystemTypes sft
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

    -- ** Minter Functions
    --

    getMinterAddress :: tk (ACC_ADDR acc)

    view_minter_contract :: UIDX_CONTRACT_ADDR acc (MINTA.ContractData sft) -> tk (MINTA.ContractData sft)
    set_minter_contract  :: UIDX_CONTRACT_ADDR acc (MINTA.ContractData sft) -> MINTA.ContractData sft -> SFT_TS sft -> tk ()

    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = getMinterAddress >>= \minterAddress -> eff_agreement_operation_uniform_mud
        (MINTA.OperationOutputF minterAddress toAddr)
        (MINTA.Mint amount)
        view_minter_contract set_minter_contract
        (\(MINTA.OperationOutputF a b) -> [a, b])
        minterMonetaryUnitData

    -- ** ITA Functions
    --

    view_ita_contract :: UIDX_CONTRACT_ADDR acc (ITA.ContractData sft) -> tk (ITA.ContractData sft)
    set_ita_contract  :: UIDX_CONTRACT_ADDR acc (ITA.ContractData sft) -> ITA.ContractData sft -> SFT_TS sft -> tk ()

    transfer :: ACC_ADDR acc -> ACC_ADDR acc -> SFT_MVAL sft -> tk ()
    transfer fromAddr toAddr amount = eff_agreement_operation_uniform_mud
        (ITA.OperationOutputF fromAddr toAddr)
        (ITA.Transfer amount)
        view_ita_contract set_ita_contract
        (\(ITA.OperationOutputF a b) -> [a, b])
        itaMonetaryUnitData

    -- ** CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    view_flow :: UIDX_CONTRACT_ADDR acc (CFA.ContractData sft) -> tk (CFA.ContractData sft)
    set_flow  :: UIDX_CONTRACT_ADDR acc (CFA.ContractData sft) -> CFA.ContractData sft -> SFT_TS sft -> tk ()

    getFlow :: ACC_ADDR acc -> ACC_ADDR acc ->  tk (CFA.ContractData sft)
    getFlow sender receiver = view_flow (CFA.OperationOutputF sender receiver)
    updateFlow :: ACC_ADDR acc -> ACC_ADDR acc -> CFA.FlowRate sft -> tk ()
    updateFlow sender receiver newFlowRate = eff_agreement_operation_uniform_mud
        -- TODO newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        (CFA.OperationOutputF sender receiver)
        (CFA.UpdateFlow newFlowRate)
        view_flow set_flow
        (\(CFA.OperationOutputF a b) -> [a, b])
        cfaMonetaryUnitData

    -- ** DFA Functions
    --

    view_decaying_flow :: UIDX_CONTRACT_ADDR acc (DFA.ContractData sft) -> tk (DFA.ContractData sft)
    set_decaying_flow  :: UIDX_CONTRACT_ADDR acc (DFA.ContractData sft) -> DFA.ContractData sft -> SFT_TS sft -> tk ()

    getDecayingFlow :: ACC_ADDR acc -> ACC_ADDR acc ->  tk (DFA.ContractData sft)
    getDecayingFlow sender receiver = view_decaying_flow (DFA.OperationOutputF sender receiver)
    updateDecayingFlow :: ACC_ADDR acc -> ACC_ADDR acc -> DFA.DistributionLimit sft -> tk ()
    updateDecayingFlow sender receiver newDistributionLimit = eff_agreement_operation_uniform_mud
        (DFA.OperationOutputF sender receiver)
        (DFA.UpdateDecayingFlow newDistributionLimit)
        view_decaying_flow set_decaying_flow
        (\(DFA.OperationOutputF a b) -> [a, b])
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
        let viewContract (PDIDX.SubscriberOperationOutputF addr) = do
                dc <- viewProportionalDistributionContract addr indexId
                sc <- viewProportionalDistributionSubscription subscriber addr indexId
                return (dc, sc)
            setContract (PDIDX.SubscriberOperationOutputF addr) (dc', sc') t = do
                overProportionalDistributionContract addr indexId (const dc') t
                overProportionalDistributionSubscription subscriber addr indexId (const sc') t
        (t, aoAccounts, PDIDX.SubscriberOperationOutput cfdaMUDΔ)  <- eff_agreement_operation_base
            (PDIDX.SubscriberOperationOutputF publisher)
            (PDIDX.Subscribe unit)
            viewContract
            setContract
        let (PDIDX.SubscriberOperationOutputF pub) = aoAccounts
        putAccount publisher (over cfdaPublisherMonetaryUnitData (<> cfdaMUDΔ) pub) t

    distributeProportionally
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- amount
        -> tk ()
    distributeProportionally publisher indexId amount = eff_agreement_operation_uniform_mud
        (IDA.PublisherOperationOutputF publisher)
        (IDA.Distribute amount)
        viewContract setContract
        (\(IDA.PublisherOperationOutputF a) -> [a])
        idaPublisherMonetaryUnitData
        where viewContract (IDA.PublisherOperationOutputF addr) =
                  viewProportionalDistributionContract addr indexId >>= \dc ->
                  return (PDIDX.dc_base dc, PDIDX.dc_ida dc)
              setContract (IDA.PublisherOperationOutputF addr) (_, dc_ida') =
                  overProportionalDistributionContract addr indexId (\dc -> dc { PDIDX.dc_ida = dc_ida' })

    distributeFlow
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- flowRate
        -> tk ()
    distributeFlow publisher indexId flowRate = eff_agreement_operation_uniform_mud
        (CFDA.PublisherOperationOutputF publisher)
        (CFDA.UpdateDistributionFlowRate flowRate)
        viewContract setContract
        (\(CFDA.PublisherOperationOutputF a) -> [a])
        cfdaPublisherMonetaryUnitData
        where viewContract (CFDA.PublisherOperationOutputF addr) =
                  viewProportionalDistributionContract addr indexId >>= \dc ->
                  return (PDIDX.dc_base dc, PDIDX.dc_cfda dc)
              setContract (CFDA.PublisherOperationOutputF addr) (_, dc_cfda') =
                  overProportionalDistributionContract addr indexId (\dc -> dc { PDIDX.dc_cfda = dc_cfda' })

-- ============================================================================
-- Internal
--

-- | Common addressing scheme for agreement contract from the universal index
type UIDX_CONTRACT_ADDR acc ac = AgreementOperationOutputF ac (ACC_ADDR acc)


-- | Effectuate an agreement operation application
eff_agreement_operation_base
    :: ( SuperfluidSystemTypes sft
       , Token tk acc sft
       , AgreementContract ac sft
       , f ~ AgreementOperationOutputF ac
       )
    => f (ACC_ADDR acc)                                -- aoAddrs
    -> AgreementOperation ac                           -- ao
    -> (f (ACC_ADDR acc) -> tk ac)                     -- acGetter
    -> (f (ACC_ADDR acc) -> ac -> SFT_TS sft -> tk ()) -- acSetter
    -> tk (SFT_TS sft, f acc, AgreementOperationOutput ac)
eff_agreement_operation_base aoAddrs ao acGetter acSetter = do
    -- load ac and accounts data
    t <- getCurrentTime
    aoAccounts <- mapM getAccount aoAddrs
    ac <- acGetter aoAddrs
    -- apply agreement operation
    let (ac', mudsΔ) = applyAgreementOperation ac ao t
    -- set new ac
    acSetter aoAddrs ac' t
    -- return mudsΔ
    return (t, aoAccounts, mudsΔ)

eff_agreement_operation_uniform_mud
    :: ( SuperfluidSystemTypes sft
       , Token tk acc sft
       , AgreementContract ac sft
       , SemigroupMonetaryUnitData mud sft
       , f ~ AgreementOperationOutputF ac
       )
    => f (ACC_ADDR acc)                                -- aoAddrs
    -> AgreementOperation ac                           -- ao
    -> (f (ACC_ADDR acc) -> tk ac)                     -- acGetter
    -> (f (ACC_ADDR acc) -> ac -> SFT_TS sft -> tk ()) -- acSetter
    -> (AgreementOperationOutput ac -> [mud])          -- mudsToList
    -> Lens' acc mud                                   -- amudLens
    -> tk ()
eff_agreement_operation_uniform_mud aoAddrs ao acGetter acSetter mudsToList amudLens = do
    (t, aoAccounts, mudsΔ) <- eff_agreement_operation_base aoAddrs ao acGetter acSetter
    -- let muds' = zipMuds aoAccounts mudsΔ
    let muds' = zipWith (<>)
                (fmap (^. amudLens) (toList aoAccounts))
                (mudsToList mudsΔ)
    mapM_ (\(addr, mud) -> putAccount addr mud t)
        (zip (toList aoAddrs)
            (uncurry (set amudLens) <$>
             zip muds' (toList aoAccounts)))
