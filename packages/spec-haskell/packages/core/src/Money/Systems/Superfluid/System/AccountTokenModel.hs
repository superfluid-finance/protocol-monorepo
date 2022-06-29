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
import           Data.Maybe
import           Data.Proxy

import           Money.Systems.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes (..))
--
import           Money.Systems.Superfluid.Concepts.Agreement
    ( Agreement (..)
    , AnyAgreementAccountData (..)
    , providedBalanceByAnyAgreement
    , updateAgreement
    )
--
import qualified Money.Systems.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Systems.Superfluid.Agreements.DecayingFlowAgreement        as DFA
import qualified Money.Systems.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Systems.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import           Money.Systems.Superfluid.Integrations.Serialization              (Serializable)


-- | SuperfluidTypes type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class SuperfluidTypes sft => Account acc sft | acc -> sft where

    addressOfAccount :: acc -> SFT_ADDR sft

    agreementOfAccount
        :: (Serializable (AgreementAccountData a) sft)
        => Proxy (AgreementAccountData a) -> acc -> Maybe (AgreementAccountData a)

    updateAgreementOfAccount
        :: (Serializable (AgreementAccountData a) sft)
        => acc -> AgreementAccountData a -> SFT_TS sft -> acc

    accountTBA :: acc -> TBA.TBAAccountData sft
    accountTBA acc = fromMaybe mempty $ agreementOfAccount (Proxy @(TBA.TBAAccountData sft)) acc

    accountCFA :: acc -> CFA.CFAAccountData sft
    accountCFA acc = fromMaybe mempty $ agreementOfAccount (Proxy @(CFA.CFAAccountData sft)) acc

    accountDFA :: acc -> DFA.DFAAccountData sft
    accountDFA acc = fromMaybe mempty $ agreementOfAccount (Proxy @(DFA.DFAAccountData sft)) acc

    agreementsOfAccount :: acc -> [AnyAgreementAccountData sft]
    agreementsOfAccount acc =
        [ MkAgreementAccountData $ accountTBA acc
        , MkAgreementAccountData $ accountCFA acc
        , MkAgreementAccountData $ accountDFA acc
        ]

    showAccountAt :: acc -> SFT_TS sft -> String

balanceOfAccountAt :: (SuperfluidTypes sft, Account acc sft) => acc -> SFT_TS sft -> SFT_RTB sft
balanceOfAccountAt holderAccount t = foldr
    ((+) . (`providedBalanceByAnyAgreement` t))
    def
    (agreementsOfAccount holderAccount)

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
    -- System operations
    --
    getCurrentTime :: tk (SFT_TS (TK_SFT tk))

    --
    -- Agreement operations
    --
    getAgreementContractData
        :: (Serializable (AgreementContractData a) (TK_SFT tk))
        => Proxy (AgreementContractData a) -> [SFT_ADDR (TK_SFT tk)] -> tk (Maybe (AgreementContractData a))

    putAgreementContractData
        :: (Serializable (AgreementContractData a) (TK_SFT tk))
        => [SFT_ADDR (TK_SFT tk)] -> SFT_TS (TK_SFT tk) -> AgreementContractData a -> tk ()

    --
    -- Account operations
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
        let (TBA.TransferPair mintFromTBA' mintToTBA') = TBA.mintLiquidity
                (TBA.TransferPair (accountTBA mintFrom) (accountTBA mintTo))
                liquidity
        putAccount minterAddress $ updateAgreementOfAccount mintFrom mintFromTBA' t
        putAccount toAddr $ updateAgreementOfAccount mintTo mintToTBA' t

    --
    -- CFA functions
    --
    calcFlowBuffer :: SFT_LQ (TK_SFT tk) -> tk (SFT_LQ (TK_SFT tk))

    getFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> tk (CFA.CFAContractData (TK_SFT tk))
    getFlow senderAddr receiverAddr = fromMaybe def <$> getAgreementContractData
        (Proxy @(CFA.CFAContractData (TK_SFT tk)))
        [senderAddr, receiverAddr]

    getDecayingFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> tk (DFA.DFAContractData (TK_SFT tk))
    getDecayingFlow senderAddr receiverAddr = fromMaybe def <$> getAgreementContractData
        (Proxy @(DFA.DFAContractData (TK_SFT tk)))
        [senderAddr, receiverAddr]

    updateFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> SFT_LQ (TK_SFT tk) -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        flowBuffer <-  calcFlowBuffer newFlowRate
        let flowACD' = CFA.CFAContractData
                { CFA.flowLastUpdatedAt = t
                , CFA.flowRate = newFlowRate
                , CFA.flowBuffer = BBS.mkBufferLiquidity flowBuffer
                }
        let (CFA.CFAParties senderFlowAAD' receiverFlowAAD') =
                updateAgreement flowACD flowACD'
                (CFA.CFAParties (accountCFA senderAccount) (accountCFA receiverAccount))
        let senderAccount' = updateAgreementOfAccount senderAccount senderFlowAAD' t
        let receiverAccount' = updateAgreementOfAccount receiverAccount receiverFlowAAD' t
        putAgreementContractData [senderAddr, receiverAddr] t flowACD'
        putAccount senderAddr senderAccount'
        putAccount receiverAddr receiverAccount'

    --
    -- DFA functions
    --
    updateDecayingFlow :: SFT_ADDR (TK_SFT tk) -> SFT_ADDR (TK_SFT tk) -> SFT_LQ (TK_SFT tk) -> tk ()
    updateDecayingFlow senderAddr receiverAddr newDistributionLimit = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getDecayingFlow senderAddr receiverAddr
        let flowACD' = DFA.DFAContractData
                { DFA.flowLastUpdatedAt = t
                , DFA.distributionLimit = newDistributionLimit
                , DFA.decayingFactor = DFA.decayingFactor flowACD
                , DFA.flowBuffer = def
                }
        let (DFA.DFAParties senderFlowAAD' receiverFlowAAD') =
                updateAgreement flowACD flowACD'
                (DFA.DFAParties (accountDFA senderAccount) (accountDFA receiverAccount))
        let senderAccount' = updateAgreementOfAccount senderAccount senderFlowAAD' t
        let receiverAccount' = updateAgreementOfAccount receiverAccount receiverFlowAAD' t
        putAgreementContractData [senderAddr, receiverAddr] t flowACD'
        putAccount senderAddr senderAccount'
        putAccount receiverAddr receiverAccount'
