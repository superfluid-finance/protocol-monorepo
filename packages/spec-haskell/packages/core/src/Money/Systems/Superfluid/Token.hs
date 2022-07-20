{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Token
    ( module Money.Systems.Superfluid.MonetaryUnit
    , Address
    , Account (..)
    , Token (..)
    ) where

import           Data.Default
import           Data.Foldable                                                (toList)
import           Data.Kind                                                    (Type)
import           Data.Maybe                                                   (fromMaybe)
import           Lens.Internal

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement    as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement    as DFA
import qualified Money.Systems.Superfluid.Agreements.InstantTransferAgreement as ITA
import qualified Money.Systems.Superfluid.Agreements.MinterAgreement          as MINTA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency      as BBS
--
import qualified Money.Systems.Superfluid.Indexes.UniversalIndexes            as UIDX
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

    --
    -- System Functions
    --

    getCurrentTime :: tk (SFT_TS sft)

    --
    -- Account Data Functions
    --

    getAccount :: ACC_ADDR acc -> tk acc

    putAccount :: ACC_ADDR acc -> acc -> tk ()

    balanceOfAccount :: ACC_ADDR acc -> tk (SFT_RTB sft)
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAt account t

    --
    -- Polymorphic Agreement Functions
    --

    changeAgreement
        :: AgreementContractData acd amud sft
        => (AgreementContractPartiesF acd) (ACC_ADDR acc)                                 -- acpAddrs
        -> AgreementOperation acd                                                         -- ao
        -> ((AgreementContractPartiesF acd) (ACC_ADDR acc) -> tk (Maybe acd))             -- acdGetter
        -> ((AgreementContractPartiesF acd) (ACC_ADDR acc) -> acd -> SFT_TS sft -> tk ()) -- acdSetter
        -> Lens' acc amud                                                                 -- amudL
        -> tk ()
    changeAgreement acpAddrs ao acdGetter acdSetter amuData = do
        -- load acd and accounts data
        t <- getCurrentTime
        acd <- fromMaybe def <$> acdGetter acpAddrs
        acpAccounts <- mapM getAccount acpAddrs
        -- apply agreement operation
        let (acd', acpΔamuds) = applyAgreementOperation acd ao t
        -- append delta to existing amuds
        let acpAMUDs' = (<>) <$> (fmap (^. amuData) acpAccounts) <*> acpΔamuds
        -- set new acd
        acdSetter acpAddrs acd' t
        -- set new amuds
        mapM_ (uncurry putAccount)
            (zip (toList acpAddrs)
                (fmap (\(amud', account) -> set amuData amud' account)
                    (zip (toList acpAMUDs') (toList acpAccounts))))

    -- * MINTA Functions
    --

    getMinterAddress :: tk (ACC_ADDR acc)

    viewMINTAContract :: CONTRACT_ACC_ADDR acc (UIDX.MINTAContractData sft) -> tk (Maybe (UIDX.MINTAContractData sft))
    setMINTAContract  :: CONTRACT_ACC_ADDR acc (UIDX.MINTAContractData sft) -> UIDX.MINTAContractData sft -> SFT_TS sft -> tk ()

    mintValue :: ACC_ADDR acc -> SFT_MVAL sft-> tk ()
    mintValue toAddr amount = do
        minterAddress <- getMinterAddress
        changeAgreement
            (MINTA.ContractPartiesF minterAddress toAddr) (MINTA.Mint amount)
            viewMINTAContract setMINTAContract mintaMonetaryUnitData

    -- * ITA Functions
    --

    viewITAContract :: CONTRACT_ACC_ADDR acc (UIDX.ITAContractData sft) -> tk (Maybe (UIDX.ITAContractData sft))
    setITAContract  :: CONTRACT_ACC_ADDR acc (UIDX.ITAContractData sft) -> UIDX.ITAContractData sft -> SFT_TS sft -> tk ()

    transfer :: CONTRACT_ACC_ADDR acc (UIDX.ITAContractData sft) -> SFT_MVAL sft -> tk ()
    transfer acpAddrs amount = do
        changeAgreement
            acpAddrs (ITA.Transfer amount)
            viewITAContract setITAContract itaMonetaryUnitData

    -- * CFA Functions
    --

    calcFlowBuffer :: SFT_MVAL sft -> tk (SFT_MVAL sft)

    viewFlow :: CONTRACT_ACC_ADDR acc (UIDX.CFAContractData sft) -> tk (Maybe (UIDX.CFAContractData sft))
    setFlow  :: CONTRACT_ACC_ADDR acc (UIDX.CFAContractData sft) -> UIDX.CFAContractData sft -> SFT_TS sft -> tk ()

    updateFlow :: CONTRACT_ACC_ADDR acc (UIDX.CFAContractData sft) -> CFA.FlowRate sft -> tk ()
    updateFlow acpAddrs newFlowRate = do
        newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        changeAgreement
            acpAddrs (CFA.UpdateFlow newFlowRate newFlowBuffer)
            viewFlow setFlow cfaMonetaryUnitData

    -- * DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ACC_ADDR acc (UIDX.DFAContractData sft) -> tk (Maybe (UIDX.DFAContractData sft))
    setDecayingFlow :: CONTRACT_ACC_ADDR acc (UIDX.DFAContractData sft) -> UIDX.DFAContractData sft -> SFT_TS sft -> tk ()

    updateDecayingFlow :: CONTRACT_ACC_ADDR acc (UIDX.DFAContractData sft) -> DFA.DistributionLimit sft -> tk ()
    updateDecayingFlow acpAddrs newDistributionLimit = do
        changeAgreement
            acpAddrs (DFA.UpdateDecayingFlow newDistributionLimit def)
            viewDecayingFlow setDecayingFlow dfaMonetaryUnitData

-- ============================================================================
-- Internal
--
type CONTRACT_ACC_ADDR acc acd = AgreementContractPartiesF acd (ACC_ADDR acc)
