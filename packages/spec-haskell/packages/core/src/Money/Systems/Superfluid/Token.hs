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

import           Money.Systems.Superfluid.Concepts
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS
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
        :: Agreement a (TK_SFT tk)
        => TS tk
        -> (AgreementContractPartiesF a) (ADDR tk)                                                -- acpAddrs
        -> AgreementOperation a                                                                   -- ao
        -> ((AgreementContractPartiesF a) (ADDR tk) -> tk (Maybe (AgreementContractData a)))      -- acdGetter
        -> ((AgreementContractPartiesF a) (ADDR tk) -> AgreementContractData a -> TS tk -> tk ()) -- acdSetter
        -> (TK_ACC tk -> AgreementAccountData a)                                                  -- aadGetter
        -> (TK_ACC tk -> AgreementAccountData a -> TS tk -> TK_ACC tk)                            -- aadSetter
        -> tk ()
    changeAgreement t acpAddrs ao acdGetter acdSetter aadGetter aadSetter = do
        acpAccounts <- mapM getAccount acpAddrs
        acd <- fromMaybe def <$> acdGetter acpAddrs
        let (acd', acpAADs') = updateAgreement acd (fmap aadGetter acpAccounts) ao
        acdSetter acpAddrs acd' t
        mapM_ (uncurry putAccount)
            (zip (toList acpAddrs)
                 (fmap (\(account, aad') -> aadSetter account aad' t) (zip (toList acpAccounts) (toList acpAADs'))))

    --
    -- TBA Functions
    --

    getMinterAddress :: tk (ADDR tk)

    viewTBAContract :: TBA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> tk (Maybe (TBA.ContractData (TK_SFT tk)))
    viewTBAContract _ = return $ Just TBA.ContractData
    setTBAContract :: TBA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> TBA.ContractData (TK_SFT tk) -> TS tk -> tk ()
    setTBAContract _ _ _ = return ()

    mintLiquidity :: ADDR tk -> LQ tk-> tk ()
    mintLiquidity toAddr amount = do
        t <- getCurrentTime
        minterAddress <- getMinterAddress
        changeAgreement
            t (TBA.ContractPartiesF minterAddress toAddr) (TBA.MintLiquidity amount)
            viewTBAContract setTBAContract viewTBAAccount setTBAAccount

    --
    -- CFA Functions
    --

    calcFlowBuffer :: LQ tk-> tk (LQ tk)

    viewFlow :: CFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> tk (Maybe (CFA.ContractData (TK_SFT tk)))
    setFlow :: CFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> CFA.ContractData (TK_SFT tk) -> TS tk -> tk ()

    updateFlow :: CFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> LQ tk-> tk ()
    updateFlow acpAddrs newFlowRate = do
        t <- getCurrentTime
        newFlowBuffer <- BBS.mkBufferLiquidity <$> calcFlowBuffer newFlowRate
        changeAgreement
            t acpAddrs (CFA.UpdateFlow newFlowRate newFlowBuffer t)
            viewFlow setFlow viewCFAAccount setCFAAccount

    --
    -- DFA Functions
    --

    viewDecayingFlow :: DFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> tk (Maybe (DFA.ContractData (TK_SFT tk)))
    setDecayingFlow :: DFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> DFA.ContractData (TK_SFT tk) -> TS tk -> tk ()

    updateDecayingFlow :: DFA.ContractPartiesF (TK_SFT tk) (ADDR tk) -> LQ tk-> tk ()
    updateDecayingFlow acpAddrs newDistributionLimit = do
        t <- getCurrentTime
        changeAgreement
            t acpAddrs (DFA.UpdateDecayingFlow newDistributionLimit def t)
            viewDecayingFlow setDecayingFlow viewDFAAccount setDFAAccount

-- ============================================================================
-- Internal
--
type TS tk = SFT_TS (TK_SFT tk)
type LQ tk = SFT_LQ (TK_SFT tk)
type RTB tk = SFT_RTB (TK_SFT tk)
type ADDR tk = ACC_ADDR (TK_ACC tk)
