{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Systems.Superfluid.System.AccountTokenModel
    ( Account (..)
    , balanceOfAccountAt
    , sumAccounts
    , Token (..)
    ) where

import           Data.Default
import           Data.Kind                                                        (Type)
import           Data.Maybe                                                       (fromMaybe)

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes (..))
--
import           Money.Systems.Superfluid.Concepts.Agreement                      (updateAgreement)
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS

-- | SuperfluidTypes type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class SuperfluidTypes sft => Account acc sft | acc -> sft where
    addressOfAccount :: acc -> SFT_ADDR sft

    --
    -- Polymorphic agreement account data functions
    --

    type AnyAgreementAccountData acc :: Type

    agreementsOfAccount :: acc -> [AnyAgreementAccountData acc]

    providedBalanceByAnyAgreement :: acc -> AnyAgreementAccountData acc -> SFT_TS sft -> SFT_RTB sft

    --
    -- Specialized agreement account data functions
    --

    viewAccountTBA :: acc -> TBA.TBAAccountData sft
    setAccountTBA :: acc -> TBA.TBAAccountData sft -> SFT_TS sft -> acc

    viewAccountCFA :: acc -> CFA.CFAAccountData sft
    setAccountCFA :: acc -> CFA.CFAAccountData sft -> SFT_TS sft -> acc

    viewAccountDFA ::  acc -> DFA.DFAAccountData sft
    setAccountDFA :: acc -> DFA.DFAAccountData sft -> SFT_TS sft -> acc

balanceOfAccountAt :: (SuperfluidTypes sft, Account acc sft) => acc -> SFT_TS sft -> SFT_RTB sft
balanceOfAccountAt account t = foldr
    ((+) . (\a -> providedBalanceByAnyAgreement account a t))
    def
    (agreementsOfAccount account)

sumAccounts :: (SuperfluidTypes sft, Account acc sft) => [acc] -> SFT_TS sft -> SFT_RTB sft
sumAccounts alist t = foldr ((+) . (`balanceOfAccountAt` t)) def alist

-- ============================================================================
-- | Token Type Class
--
-- Naming conventions:
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
--
class (Monad tk, SuperfluidTypes (TK_SFT tk), Account (TK_ACC tk) (TK_SFT tk)) => Token tk where

    type TK_SFT tk :: Type
    type TK_ACC tk :: Type

    --
    -- System functions
    --
    getCurrentTime :: tk (SFT_TS (TK_SFT tk))

    --
    -- Polymorphic agreement contract data functions
    --
    -- mkAnyAgreementContractData
    --     :: ( Agreement a
    --        , DistributionForAgreement a ~ TK_SFT tk
    --        )
    --     => AgreementContractData a
    --     -> tk (AnyAgreementContractData (TK_SFT tk))

    -- putAnyAgreementContractData
    --     :: [SFT_ADDR (TK_SFT tk)]
    --     -> SFT_TS (TK_SFT tk)
    --     -> AnyAgreementContractData (TK_SFT tk)
    --     -> tk ()

    -- getAnyAgreementContractData
    --     :: ( Agreement a
    --        , DistributionForAgreement a ~ TK_SFT tk
    --        )
    --     => Proxy (AgreementContractData a)
    --     -> [SFT_ADDR (TK_SFT tk)]
    --     -> tk (Maybe (AnyAgreementContractData (TK_SFT tk)))

    --
    -- Account data
    --
    getAccount :: SFT_ADDR (TK_SFT tk) -> tk (TK_ACC tk)

    putAccount :: SFT_ADDR (TK_SFT tk) -> TK_ACC tk -> tk ()

    balanceOfAccount :: SFT_ADDR (TK_SFT tk) -> tk (SFT_RTB (TK_SFT tk))
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAccountAt account t

    --
    -- TBA functions
    --
    getMinterAddress :: tk (SFT_ADDR (TK_SFT tk))

    mintLiquidity :: SFT_ADDR (TK_SFT tk) -> SFT_LQ (TK_SFT tk) -> tk ()
    mintLiquidity toAddr liquidity = do
        t <- getCurrentTime
        minterAddress <- getMinterAddress
        mintFrom <- getAccount minterAddress
        mintTo <- getAccount toAddr
        let (TBA.TBAParties mintFromTBA' mintToTBA') = TBA.mintLiquidity
                (TBA.TBAParties (viewAccountTBA mintFrom) (viewAccountTBA mintTo))
                liquidity
        putAccount minterAddress $ setAccountTBA mintFrom mintFromTBA' t
        putAccount toAddr $ setAccountTBA mintTo mintToTBA' t

    --
    -- CFA functions
    --
    calcFlowBuffer :: SFT_LQ (TK_SFT tk) -> tk (SFT_LQ (TK_SFT tk))

    viewFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> tk (Maybe (CFA.CFAContractData (TK_SFT tk)))
    setFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> CFA.CFAContractData (TK_SFT tk) -> SFT_TS (TK_SFT tk) -> tk ()

    updateFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> SFT_LQ (TK_SFT tk) -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- fromMaybe def <$> viewFlow senderAddr receiverAddr
        flowBuffer <-  calcFlowBuffer newFlowRate
        let flowACD' = CFA.CFAContractData
                { CFA.flowLastUpdatedAt = t
                , CFA.flowRate = newFlowRate
                , CFA.flowBuffer = BBS.mkBufferLiquidity flowBuffer
                }
        let (CFA.CFAParties senderFlowAAD' receiverFlowAAD') =
                updateAgreement flowACD flowACD'
                (CFA.CFAParties (viewAccountCFA senderAccount) (viewAccountCFA receiverAccount))
        let senderAccount' = setAccountCFA senderAccount senderFlowAAD' t
        let receiverAccount' = setAccountCFA receiverAccount receiverFlowAAD' t
        setFlow senderAddr receiverAddr flowACD' t
        putAccount senderAddr senderAccount'
        putAccount receiverAddr receiverAccount'

    --
    -- DFA functions
    --
    viewDecayingFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> tk (Maybe (DFA.DFAContractData (TK_SFT tk)))
    setDecayingFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> DFA.DFAContractData (TK_SFT tk) -> SFT_TS (TK_SFT tk) -> tk ()

    updateDecayingFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> SFT_LQ (TK_SFT tk) -> tk ()
    updateDecayingFlow senderAddr receiverAddr newDistributionLimit = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- fromMaybe def <$> viewDecayingFlow senderAddr receiverAddr
        let flowACD' = DFA.DFAContractData
                { DFA.flowLastUpdatedAt = t
                , DFA.distributionLimit = newDistributionLimit
                , DFA.decayingFactor = DFA.decayingFactor flowACD
                , DFA.flowBuffer = def
                }
        let (DFA.DFAParties senderFlowAAD' receiverFlowAAD') =
                updateAgreement flowACD flowACD'
                (DFA.DFAParties (viewAccountDFA senderAccount) (viewAccountDFA receiverAccount))
        let senderAccount' = setAccountDFA senderAccount senderFlowAAD' t
        let receiverAccount' = setAccountDFA receiverAccount receiverFlowAAD' t
        setDecayingFlow senderAddr receiverAddr flowACD' t
        putAccount senderAddr senderAccount'
        putAccount receiverAddr receiverAccount'
