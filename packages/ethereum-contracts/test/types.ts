import {BigNumber} from "ethers";

import {
    ConstantFlowAgreementV1,
    GeneralDistributionAgreementV1,
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
                                    indexValue: BigNumber;
                                    totalUnitsApproved: BigNumber;
                                    totalUnitsPending: BigNumber;
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
                                units: BigNumber;
                                _syncedIndexValue: BigNumber;
                                pendingDistribution: BigNumber;
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
                        flowRate: BigNumber;
                    };
                };
            };
        };
    };
}

export interface BenchmarkingData {
    testName: string;
    totalTime: number;
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
    gda: GeneralDistributionAgreementV1;
    governance: TestGovernance;
    ISuperToken: ISuperToken;
    resolver: Resolver;
}

export const CUSTOM_ERROR_CODES = {
    APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK: 10,
    APP_RULE_NO_CRITICAL_SENDER_ACCOUNT: 11,
    APP_RULE_NO_CRITICAL_RECEIVER_ACCOUNT: 12,
    APP_RULE_CTX_IS_READONLY: 20,
    APP_RULE_CTX_IS_NOT_CLEAN: 21,
    APP_RULE_CTX_IS_MALFORMATED: 22,
    APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED: 30,
    APP_RULE_COMPOSITE_APP_IS_JAILED: 31,
    APP_RULE_MAX_APP_LEVEL_REACHED: 40,
};

export type CustomErrorCodeType = typeof CUSTOM_ERROR_CODES;
