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

import           Money.Systems.Superfluid.Concepts.SuperfluidDistribution                (SuperfluidDistribution (..))
--
import           Money.Systems.Superfluid.Concepts.Agreement                      (updateAgreement)
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS

-- | SuperfluidDistribution type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class SuperfluidDistribution sfd => Account acc sfd | acc -> sfd where
    addressOfAccount :: acc -> SFT_ADDR sfd

    --
    -- Polymorphic agreement account data functions
    --

    type AnyAgreementAccountData acc :: Type

    agreementsOfAccount :: acc -> [AnyAgreementAccountData acc]

    providedBalanceByAnyAgreement :: acc -> AnyAgreementAccountData acc -> SFT_TS sfd -> SFT_RTB sfd

    --
    -- Specialized agreement account data functions
    --

    viewAccountTBA :: acc -> TBA.TBAAccountData sfd
    setAccountTBA :: acc -> TBA.TBAAccountData sfd -> SFT_TS sfd -> acc

    viewAccountCFA :: acc -> CFA.CFAAccountData sfd
    setAccountCFA :: acc -> CFA.CFAAccountData sfd -> SFT_TS sfd -> acc

    viewAccountDFA ::  acc -> DFA.DFAAccountData sfd
    setAccountDFA :: acc -> DFA.DFAAccountData sfd -> SFT_TS sfd -> acc

balanceOfAccountAt :: (SuperfluidDistribution sfd, Account acc sfd) => acc -> SFT_TS sfd -> SFT_RTB sfd
balanceOfAccountAt account t = foldr
    ((+) . (\a -> providedBalanceByAnyAgreement account a t))
    def
    (agreementsOfAccount account)

sumAccounts :: (SuperfluidDistribution sfd, Account acc sfd) => [acc] -> SFT_TS sfd -> SFT_RTB sfd
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
class (Monad tk, SuperfluidDistribution (TK_SFT tk), Account (TK_ACC tk) (TK_SFT tk)) => Token tk where

    type TK_SFT tk :: Type
    type TK_ACC tk :: Type

    --
    -- System functions
    --
    getCurrentTime :: tk (SFT_TS (TK_SFT tk))

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
    mintLiquidity toAddr amount = do
        t <- getCurrentTime
        minterAddress <- getMinterAddress
        mintFrom <- getAccount minterAddress
        mintTo <- getAccount toAddr
        let (_, TBA.TBAPartiesF mintFromACD' mintToACD') = updateAgreement
                TBA.TBAContractData
                (TBA.TBAPartiesF (viewAccountTBA mintFrom) (viewAccountTBA mintTo))
                (TBA.MintLiquidity amount)
        putAccount minterAddress $ setAccountTBA mintFrom mintFromACD' t
        putAccount toAddr $ setAccountTBA mintTo mintToACD' t

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
        newFlowBuffer <- BBS.mkBufferLiquidity <$> calcFlowBuffer newFlowRate
        let (flowACD', CFA.CFAPartiesF senderFlowAAD' receiverFlowAAD') = updateAgreement
                flowACD
                (CFA.CFAPartiesF (viewAccountCFA senderAccount) (viewAccountCFA receiverAccount))
                (CFA.Updatelow newFlowRate newFlowBuffer t)
        setFlow senderAddr receiverAddr flowACD' t
        putAccount senderAddr $ setAccountCFA senderAccount senderFlowAAD' t
        putAccount receiverAddr $ setAccountCFA receiverAccount receiverFlowAAD' t

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
        let (flowACD', DFA.DFAPartiesF senderFlowAAD' receiverFlowAAD') = updateAgreement
                flowACD
                (DFA.DFAPartiesF (viewAccountDFA senderAccount) (viewAccountDFA receiverAccount))
                (DFA.UpdateDecayingFlow newDistributionLimit def t)
        setDecayingFlow senderAddr receiverAddr flowACD' t
        putAccount senderAddr $ setAccountDFA senderAccount senderFlowAAD' t
        putAccount receiverAddr $ setAccountDFA receiverAccount receiverFlowAAD' t
