{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Token
    ( module Money.Systems.Superfluid.MonetaryUnit
    , Address
    , Account (..)
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
import qualified Money.Systems.Superfluid.Agreements.Indexes.ProportionalDistributionIndex as PDIDX
import qualified Money.Systems.Superfluid.Agreements.InstantDistributionAgreement          as IDA
import qualified Money.Systems.Superfluid.Agreements.UniversalIndex                        as UIDX
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

    updateUniversalIndex
        :: AgreementOperation ao acd amud sft
        => (AgreementOperationPartiesF ao) (ACC_ADDR acc)                                 -- aopsAddrs
        -> ao                                                                             -- ao
        -> ((AgreementOperationPartiesF ao) (ACC_ADDR acc) -> tk (Maybe acd))             -- acdGetter
        -> ((AgreementOperationPartiesF ao) (ACC_ADDR acc) -> acd -> SFT_TS sft -> tk ()) -- acdSetter
        -> Lens' acc amud                                                                 -- amudL
        -> tk ()
    updateUniversalIndex aopsAddrs ao acdGetter acdSetter amuData = do
        -- load acd and accounts data
        t <- getCurrentTime
        acd <- fromMaybe def <$> acdGetter aopsAddrs
        aopsAccounts <- mapM getAccount aopsAddrs
        -- apply agreement operation
        let (acd', aopsΔamuds) = applyAgreementOperation ao acd t
        -- append delta to existing amuds
        let amuds' = zipWith (<>) (fmap (^. amuData) (toList aopsAccounts)) (toList aopsΔamuds)
        -- set new acd
        acdSetter aopsAddrs acd' t
        -- set new amuds
        mapM_ (\(addr, amud) -> putAccount addr amud t)
            (zip (toList aopsAddrs)
                (fmap (\(amud', account) -> set amuData amud' account)
                    (zip amuds' (toList aopsAccounts))))

    -- ** Minter Functions
    --

    getMinterAddress :: tk (ACC_ADDR acc)

    viewMinterContract :: CONTRACT_ACC_ADDR acc (UIDX.MinterOperation sft) -> tk (Maybe (UIDX.MinterContractData sft))
    setMinterContract  :: CONTRACT_ACC_ADDR acc (UIDX.MinterOperation sft) -> UIDX.MinterContractData sft -> SFT_TS sft -> tk ()

    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = do
        minterAddress <- getMinterAddress
        updateUniversalIndex
            (UIDX.MinterOperationPartiesF minterAddress toAddr) (UIDX.Mint amount)
            viewMinterContract setMinterContract minterMonetaryUnitData

    -- ** ITA Functions
    --

    viewITAContract :: CONTRACT_ACC_ADDR acc (UIDX.ITAOperation sft) -> tk (Maybe (UIDX.ITAContractData sft))
    setITAContract  :: CONTRACT_ACC_ADDR acc (UIDX.ITAOperation sft) -> UIDX.ITAContractData sft -> SFT_TS sft -> tk ()

    transfer :: CONTRACT_ACC_ADDR acc (UIDX.ITAOperation sft) -> SFT_MVAL sft -> tk ()
    transfer aopsAddrs amount = do
        updateUniversalIndex
            aopsAddrs (UIDX.Transfer amount)
            viewITAContract setITAContract itaMonetaryUnitData

    -- ** CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    viewFlow :: CONTRACT_ACC_ADDR acc (UIDX.CFAOperation sft) -> tk (Maybe (UIDX.CFAContractData sft))
    setFlow  :: CONTRACT_ACC_ADDR acc (UIDX.CFAOperation sft) -> UIDX.CFAContractData sft -> SFT_TS sft -> tk ()

    updateFlow :: CONTRACT_ACC_ADDR acc (UIDX.CFAOperation sft) -> UIDX.FlowRate sft -> tk ()
    updateFlow aopsAddrs newFlowRate = do
        newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        updateUniversalIndex
            aopsAddrs (UIDX.UpdateFlow newFlowRate newFlowBuffer)
            viewFlow setFlow cfaMonetaryUnitData

    -- ** DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ACC_ADDR acc (UIDX.DFAOperation sft) -> tk (Maybe (UIDX.DFAContractData sft))
    setDecayingFlow  :: CONTRACT_ACC_ADDR acc (UIDX.DFAOperation sft) -> UIDX.DFAContractData sft -> SFT_TS sft -> tk ()

    updateDecayingFlow :: CONTRACT_ACC_ADDR acc (UIDX.DFAOperation sft) -> UIDX.DistributionLimit sft -> tk ()
    updateDecayingFlow aopsAddrs newDistributionLimit = do
        updateUniversalIndex
            aopsAddrs (UIDX.UpdateDecayingFlow newDistributionLimit def)
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
        index <- fromMaybe def <$> viewProportionalDistributionContract publisher indexId
        sub  <- fromMaybe def <$> viewProportionalDistributionSubscription subscriber publisher indexId
        let aod = PDIDX.SubscriberData index sub
        let (PDIDX.SubscriberData index' sub', _) = applyAgreementOperation (IDA.Subscribe unit) aod t
        setProportionalDistributionContract publisher indexId index' t
        setProportionalDistributionSubscription subscriber publisher indexId sub' t

    distributeProportionally
        :: ACC_ADDR acc                    -- publisher
        -> ProportionalDistributionIndexID -- indexId
        -> SFT_MVAL sft                    -- amount
        -> tk ()
    distributeProportionally publisher indexId amount = do
        -- load acd and accounts data
        t <- getCurrentTime
        acc <- getAccount publisher
        index <- fromMaybe def <$> viewProportionalDistributionContract publisher indexId
        let (index', IDA.IDAOPublisherOperationPartiesF amudΔ) =
                applyAgreementOperation (IDA.Distribute amount) index t
        setProportionalDistributionContract publisher indexId index' t
        putAccount publisher (over idaPublisherMonetaryUnitData (<> amudΔ) acc) t

-- ============================================================================
-- Internal
--
type CONTRACT_ACC_ADDR acc ao = AgreementOperationPartiesF ao (ACC_ADDR acc)
