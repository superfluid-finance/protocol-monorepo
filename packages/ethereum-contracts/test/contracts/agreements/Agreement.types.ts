import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {BigNumber} from "ethers";

import {SuperToken} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {MFAParams} from "../utils/MFASupport";

/////////////////////
// AgreementHelper //
/////////////////////
export interface CallAgreementParams {
    readonly signer: SignerWithAddress;
    readonly agreementAddress: string;
    readonly callData: string;
    readonly userData?: string;
}

export interface CallAppActionParams {
    readonly signer: SignerWithAddress;
    readonly appAddress: string;
    readonly callData: string;
}

export interface UpdateFlowOperatorPermissionsParams {
    readonly superToken: SignerWithAddress;
    readonly flowOperator: string;
    readonly permissions: string;
    readonly flowRateAllowance: string;
}

export interface ModifyFlowCallDataParams {
    readonly superToken: string;
    readonly receiver: string;
    readonly type: string;
    readonly flowRate?: BigNumber;
    readonly sender: string;
}

///////////
// CFAv1 //
///////////
export interface AccountFlowInfo {
    readonly flowRate: BigNumber;
    readonly deposit: BigNumber;
}

export interface FlowInfo {
    readonly timestamp: BigNumber;
    readonly flowRate: BigNumber;
    readonly deposit: BigNumber;
    readonly owedDeposit: BigNumber;
}

export interface FlowParams {
    readonly sender: string;
    readonly receiver: string;
    readonly notTouched?: boolean;
}

export interface RealtimeBalance {
    availableBalance: BigNumber;
    deposit: BigNumber;
    owedDeposit: BigNumber;
    timestamp: BigNumber;
    description?: string;
}

interface ShouldChangeFlowBaseParams {
    readonly testenv: TestEnvironment;
    readonly superToken: SuperToken;
    readonly sender: string;
    readonly receiver: string;
    readonly flowRate: BigNumber;
    readonly flowOperator: string;
    readonly mfa?: MFAParams;
}
export type ChangeFlowBaseParams = Omit<
    ShouldChangeFlowBaseParams,
    "flowOperator"
>;
export type OperatorChangeFlowParams = ShouldChangeFlowBaseParams;

export type DeleteFlowParams = Omit<
    ShouldChangeFlowBaseParams,
    "flowOperator" | "flowRate"
> & {
    readonly by: string;
    readonly accountFlowInfo?: AccountFlowInfo;
};

export interface OperatorPermissionsBaseParams {
    readonly testenv: TestEnvironment;
    readonly superToken: string;
    readonly flowOperator: string;
    readonly ctx: string;
    readonly signer: SignerWithAddress;
}

export interface ChangeFlowParams extends ChangeFlowBaseParams {
    fn: string;
    userData?: string;
    by?: string;
    accountFlowInfo?: AccountFlowInfo;
}

export interface CreateLiquidationTestParams {
    by: string;
    receiver: string;
    seconds: BigNumber;
    sender: string;
    titlePrefix: string;
}

export interface CreateBailoutTestParams extends CreateLiquidationTestParams {
    allowCriticalAccount?: boolean;
}

export interface TestLiquidationParams {
    allowCriticalAccount?: boolean;
    by: string;
    isBailout?: boolean;
    receiver: string;
    seconds: BigNumber;
    sender: string;
    shouldSkipTimeTravel?: boolean;
    solvencyStatuses: {
        preIsCritical: boolean;
        preIsSolvent: boolean;
        postIsCritical: boolean;
        postIsSolvent: boolean;
    };
}

export interface VerifyOptions {
    time?: BigNumber;
    allowCriticalAccount?: boolean;
    tokenSymbol?: string;
    description?: string;
}

///////////
// IDAv1 //
///////////
export interface IDAIndexData {
    exist: boolean;
    indexValue: BigNumber;
    totalUnitsApproved: BigNumber;
    totalUnitsPending: BigNumber;
}

export interface PartialIDAIndexData {
    exist?: boolean;
    indexValue?: BigNumber;
    totalUnitsApproved?: BigNumber;
    totalUnitsPending?: BigNumber;
}

export interface IDASubscriptionData {
    exist: boolean;
    approved?: boolean;
    units?: BigNumber;
    _syncedIndexValue?: BigNumber;
    pendingDistribution?: BigNumber;
}

export interface IDABaseParams {
    testenv: TestEnvironment;
    superToken: SuperToken;
    publisher?: string;
    indexId: string;
}
