import {BigNumber} from "ethers";

import {
    ConstantFlowAgreementV1,
    IERC1820Registry,
    InstantDistributionAgreementV1,
    ISuperToken,
    Resolver,
    SuperfluidMock,
    TestGovernance,
} from "../typechain-types";

import {FlowInfo} from "./contracts/agreements/Agreement.types";

export interface RealtimeBalance {
    availableBalance: BigNumber;
    deposit: BigNumber;
    owedDeposit: BigNumber;
    timestamp: BigNumber;
    description?: string;
}
export interface TestEnvironmentData {
    moreAliases: {
        [alias: string]: string;
    };
    tokens: {
        [superToken: string]: {
            accounts: {
                [account: string]: {
                    balanceSnapshot: RealtimeBalance;
                    expectedBalanceDelta: string;
                    cfa: {
                        flowInfo: FlowInfo;
                    };
                    ida: {
                        indices: {
                            [indexId: string]: {
                                data: {
                                    exist: boolean;
                                    indexValue: string;
                                    totalUnitsApproved: string;
                                    totalUnitsPending: string;
                                };
                                subscribers: {
                                    [subId: string]: {
                                        approved: boolean;
                                    };
                                };
                            };
                        };
                        subscriptions: {
                            [subId: string]: {
                                exist: boolean;
                                approved: boolean;
                                units: string;
                                _syncedIndexValue: string;
                                pendingDistribution: string;
                            };
                        };
                    };
                };
            };
            cfa: {
                flows: {
                    [id: string]: {
                        sender: string;
                        receiver: string;
                        flowRate: string;
                    };
                };
            };
        };
    };
}

export interface TestEnvironmentPlotData {
    enabled: boolean;
    observedAccounts: string[];
    tokens: {
        [superTokenAddress: string]: {
            accountBalanceSnapshots: {
                [account: string]: RealtimeBalance[];
            };
        };
    };
}

export interface TestEnvironmentConfigs {
    readonly INIT_BALANCE: BigNumber;
    readonly AUM_DUST_AMOUNT: BigNumber;
    readonly LIQUIDATION_PERIOD: BigNumber;
    readonly PATRICIAN_PERIOD: BigNumber;
    readonly FLOW_RATE1: BigNumber;
    readonly MINIMUM_DEPOSIT: BigNumber;
}

export interface TestEnvironmentConstants {
    readonly ZERO_ADDRESS: string;
    readonly ZERO_BYTES32: string;
    readonly MAX_UINT256: BigNumber;
    readonly MAX_INT256: BigNumber;
    readonly MIN_INT256: BigNumber;
    readonly MAXIMUM_FLOW_RATE: BigNumber;
    readonly APP_LEVEL_FINAL: number;
    readonly APP_LEVEL_SECOND: number;
}

export interface TestEnvironmentContracts {
    erc1820: IERC1820Registry;
    superfluid: SuperfluidMock;
    cfa: ConstantFlowAgreementV1;
    ida: InstantDistributionAgreementV1;
    governance: TestGovernance;
    ISuperToken: ISuperToken;
    resolver: Resolver;
}

export const CUSTOM_ERROR_CODE = {
    APP_RULE_REGISTRATION_ONLY_IN_CONSTRUCTOR: 1,
    APP_RULE_NO_REGISTRATION_FOR_EOA: 2,
    APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK: 10,
    APP_RULE_NO_CRITICAL_SENDER_ACCOUNT: 11,
    APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT: 12,
    APP_RULE_CTX_IS_READONLY: 20,
    APP_RULE_CTX_IS_NOT_CLEAN: 21,
    APP_RULE_CTX_IS_MALFORMATED: 22,
    APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED: 30,
    APP_RULE_COMPOSITE_APP_IS_JAILED: 31,
    APP_RULE_MAX_APP_LEVEL_REACHED: 40,

    CFA_FLOW_ALREADY_EXISTS: 1000,
    CFA_FLOW_DOES_NOT_EXIST: 1001,
    CFA_INSUFFICIENT_BALANCE: 1100,
    CFA_ZERO_ADDRESS_SENDER: 1500,
    CFA_ZERO_ADDRESS_RECEIVER: 1501,

    IDA_INDEX_ALREADY_EXISTS: 2000,
    IDA_INDEX_DOES_NOT_EXIST: 2001,
    IDA_SUBSCRIPTION_DOES_NOT_EXIST: 2002,
    IDA_SUBSCRIPTION_ALREADY_APPROVED: 2003,
    IDA_SUBSCRIPTION_IS_NOT_APPROVED: 2004,
    IDA_INSUFFICIENT_BALANCE: 2100,
    IDA_ZERO_ADDRESS_SUBSCRIBER: 2500,

    HOST_AGREEMENT_ALREADY_REGISTERED: 3000,
    HOST_AGREEMENT_IS_NOT_REGISTERED: 3001,
    HOST_SUPER_APP_ALREADY_REGISTERED: 3002,
    HOST_MUST_BE_CONTRACT: 3200,
    HOST_ONLY_LISTED_AGREEMENT: 3300,

    SF_GOV_MUST_BE_CONTRACT: 4200,

    SF_TOKEN_AGREEMENT_ALREADY_EXISTS: 5000,
    SF_TOKEN_AGREEMENT_DOES_NOT_EXIST: 5001,
    SF_TOKEN_BURN_INSUFFICIENT_BALANCE: 5100,
    SF_TOKEN_MOVE_INSUFFICIENT_BALANCE: 5101,
    SF_TOKEN_ONLY_LISTED_AGREEMENT: 5300,
    SF_TOKEN_ONLY_HOST: 5400,

    SUPER_TOKEN_ONLY_HOST: 6400,
    SUPER_TOKEN_APPROVE_FROM_ZERO_ADDRESS: 6500,
    SUPER_TOKEN_APPROVE_TO_ZERO_ADDRESS: 6501,
    SUPER_TOKEN_BURN_FROM_ZERO_ADDRESS: 6502,
    SUPER_TOKEN_MINT_TO_ZERO_ADDRESS: 6503,
    SUPER_TOKEN_TRANSFER_FROM_ZERO_ADDRESS: 6504,
    SUPER_TOKEN_TRANSFER_TO_ZERO_ADDRESS: 6505,
    SUPER_TOKEN_FACTORY_ONLY_HOST: 7400,
    SUPER_TOKEN_FACTORY_ZERO_ADDRESS: 7500,

    AGREEMENT_BASE_ONLY_HOST: 8400,
};

export type CustomErrorCodeType = typeof CUSTOM_ERROR_CODE;
