{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.Token
    ( module Money.Systems.Superfluid.MoneyUnit
    , Address
    , Account (..)
    , Token (..)
    ) where

import           Data.Default
import           Data.Foldable                                                    (toList)
import           Data.Kind                                                        (Type)
import           Data.Maybe                                                       (fromMaybe)
import           Lens.Micro

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import qualified Money.Systems.Superfluid.Indexes.Universalndexes                 as UIDX
--
import           Money.Systems.Superfluid.MoneyUnit

-- | Address type class.
--
-- Notional conventions:
--  * Type name: addr
class Eq addr => Address addr

-- | MoneyUnit type class.
--
-- Notional conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *MoneyUnit
class (SuperfluidTypes sft, MoneyUnit acc sft) => Account acc sft | acc -> sft where
    type ACC_ADDR acc :: Type
    addressOfAccount :: acc -> ACC_ADDR acc

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
--   * and agreement (TBA/CFA/GDA) operations.
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
        :: ( AgreementMonetaryUnitData amu (TK_SFT tk)
           , AgreementContractData acd amu (TK_SFT tk)
           )
        => TS tk
        -> (AgreementContractPartiesF acd) (ADDR tk)                             -- acpAddrs
        -> AgreementOperation acd                                                -- ao
        -> ((AgreementContractPartiesF acd) (ADDR tk) -> tk (Maybe acd))         -- acdGetter
        -> ((AgreementContractPartiesF acd) (ADDR tk) -> acd -> TS tk -> tk ())  -- acdSetter
        -> Lens' (TK_ACC tk) amu                                                 -- amuLens
        -> tk ()
    changeAgreement t acpAddrs ao acdGetter acdSetter amuLens = do
        acpAccounts <- mapM getAccount acpAddrs
        acd <- fromMaybe def <$> acdGetter acpAddrs
        let (acd', acpAADs') = applyAgreementOperation acd (fmap (^. amuLens) acpAccounts) ao
        acdSetter acpAddrs acd' t
        mapM_ (uncurry putAccount)
            (zip (toList acpAddrs)
                 (fmap (\(aad', account) -> set amuLens aad' account) (zip (toList acpAADs') (toList acpAccounts))))

    --
    -- TBA Functions
    --

    getMinterAddress :: tk (ADDR tk)

    viewTBAContract :: CONTRACT_ADDR tk (UIDX.TBAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.TBAContractData (TK_SFT tk)))
    setTBAContract  :: CONTRACT_ADDR tk (UIDX.TBAContractData (TK_SFT tk)) -> UIDX.TBAContractData (TK_SFT tk) -> TS tk -> tk ()

    mintLiquidity :: ADDR tk -> LQ tk-> tk ()
    mintLiquidity toAddr amount = do
        t <- getCurrentTime
        minterAddress <- getMinterAddress
        changeAgreement
            t (TBA.ContractPartiesF minterAddress toAddr) (TBA.MintLiquidity amount)
            viewTBAContract setTBAContract tbaMonetaryUnitLens

    --
    -- CFA Functions
    --

    calcFlowBuffer :: LQ tk-> tk (LQ tk)

    viewFlow :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.CFAContractData (TK_SFT tk)))
    setFlow  :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> UIDX.CFAContractData (TK_SFT tk) -> TS tk -> tk ()

    updateFlow :: CONTRACT_ADDR tk (UIDX.CFAContractData (TK_SFT tk)) -> LQ tk-> tk ()
    updateFlow acpAddrs newFlowRate = do
        t <- getCurrentTime
        newFlowBuffer <- BBS.mkBufferLiquidity <$> calcFlowBuffer newFlowRate
        changeAgreement
            t acpAddrs (CFA.UpdateFlow newFlowRate newFlowBuffer t)
            viewFlow setFlow cfaMonetaryUnitLens

    --
    -- DFA Functions
    --

    viewDecayingFlow :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> tk (Maybe (UIDX.DFAContractData (TK_SFT tk)))
    setDecayingFlow  :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> UIDX.DFAContractData (TK_SFT tk) -> TS tk -> tk ()

    updateDecayingFlow :: CONTRACT_ADDR tk (UIDX.DFAContractData (TK_SFT tk)) -> LQ tk-> tk ()
    updateDecayingFlow acpAddrs newDistributionLimit = do
        t <- getCurrentTime
        changeAgreement
            t acpAddrs (DFA.UpdateDecayingFlow newDistributionLimit def t)
            viewDecayingFlow setDecayingFlow dfaMonetaryUnitLens

-- ============================================================================
-- Internal
--
type TS tk = SFT_TS (TK_SFT tk)
type LQ tk = SFT_LQ (TK_SFT tk)
type RTB tk = SFT_RTB (TK_SFT tk)
type ADDR tk = ACC_ADDR (TK_ACC tk)
type CONTRACT_ADDR tk acd = AgreementContractPartiesF acd (ADDR tk)
