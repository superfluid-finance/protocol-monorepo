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
      , SuperfluidTypes (TK_SFT tk)
      , Account (TK_ACC tk) (TK_SFT tk)
      ) => Token tk where

    type TK_SFT tk :: Type
    type TK_ACC tk :: Type

    --
    -- System Functions
    --

    getCurrentTime :: tk (TS tk)

    --
    -- Account Data Functions
    --

    getAccount :: ADDR tk -> tk (TK_ACC tk)

    putAccount :: ADDR tk -> TK_ACC tk -> tk ()

    balanceOfAccount :: ADDR tk -> tk (RTB tk)
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAt account t

    --
    -- Polymorphic Agreement Functions
    --

    changeAgreement
        :: AgreementContractData acd amu (TK_SFT tk)
        => (AgreementContractPartiesF acd) (ADDR tk)                             -- acpAddrs
        -> AgreementOperation acd                                                -- ao
        -> ((AgreementContractPartiesF acd) (ADDR tk) -> tk (Maybe acd))         -- acdGetter
        -> ((AgreementContractPartiesF acd) (ADDR tk) -> acd -> TS tk -> tk ())  -- acdSetter
        -> Lens' (TK_ACC tk) amu                                                 -- amu
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

    --
    -- ITA Functions
    --

    getMinterAddress :: tk (ADDR tk)

    viewITAContract :: CONTRACT_ADDR tk (UIDX.ITAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.ITAContractData (TK_SFT tk)))
    setITAContract  :: CONTRACT_ADDR tk (UIDX.ITAContractData (TK_SFT tk)) -> UIDX.ITAContractData (TK_SFT tk) -> TS tk -> tk ()

    mintValue :: ADDR tk -> MVAL tk-> tk ()
    mintValue toAddr amount = do
        minterAddress <- getMinterAddress
        changeAgreement
            (ITA.ContractPartiesF minterAddress toAddr) (ITA.Mint amount)
            viewITAContract setITAContract itaMonetaryUnitData

    --
    -- CFA Functions
    --

    calcFlowBuffer :: MVAL tk -> tk (MVAL tk)

    viewFlow :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.CFAContractData (TK_SFT tk)))
    setFlow  :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> UIDX.CFAContractData (TK_SFT tk) -> TS tk -> tk ()

    updateFlow :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> CFA.FlowRate (TK_SFT tk) -> tk ()
    updateFlow acpAddrs newFlowRate = do
        newFlowBuffer <- BBS.mkBufferValue <$> calcFlowBuffer newFlowRate
        changeAgreement
            acpAddrs (CFA.UpdateFlow newFlowRate newFlowBuffer)
            viewFlow setFlow cfaMonetaryUnitData

    --
    -- DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.DFAContractData (TK_SFT tk)))
    setDecayingFlow  :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> UIDX.DFAContractData (TK_SFT tk) -> TS tk -> tk ()

    updateDecayingFlow :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> DFA.DistributionLimit (TK_SFT tk) -> tk ()
    updateDecayingFlow acpAddrs newDistributionLimit = do
        changeAgreement
            acpAddrs (DFA.UpdateDecayingFlow newDistributionLimit def)
            viewDecayingFlow setDecayingFlow dfaMonetaryUnitData

-- ============================================================================
-- Internal
--
type TS tk = SFT_TS (TK_SFT tk)
type MVAL tk = SFT_MVAL (TK_SFT tk)
type RTB tk = SFT_RTB (TK_SFT tk)
type ADDR tk = ACC_ADDR (TK_ACC tk)
type CONTRACT_ADDR tk acd = AgreementContractPartiesF acd (ADDR tk)
