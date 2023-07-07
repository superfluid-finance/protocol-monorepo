import {
    GDAv1Forwarder,
    GDAv1Forwarder__factory,
    IGeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1__factory,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ethers } from "ethers";

import Host from "./Host";
import SuperfluidAgreement from "./SuperfluidAgreement";
import {
    ConnectPoolParams,
    CreatePoolParams,
    DisconnectPoolParams,
    DistributeFlowParams,
    DistributeParams,
    EstimateDistributionActualAmountParams,
    EstimateFlowDistributionActualFlowRateParams,
    FlowDistributionActualFlowRateData,
    GDAGetFlowRateParams,
    GDAGetNetFlowParams,
    GetPoolAdjustmentFlowRateParams,
    IsMemberConnectedParams,
    IsPoolParams,
    PoolAdjustmentFlowInfo,
} from "./interfaces";
import { normalizeAddress } from "./utils";

const gdaInterface = IGeneralDistributionAgreementV1__factory.createInterface();

/**
 * General Distribution Agreement V1 Helper Class
 * @description A helper class to interact with the GDAV1 contract.
 */
export default class GeneralDistributionAgreementV1 extends SuperfluidAgreement {
    readonly host: Host;
    readonly contract: IGeneralDistributionAgreementV1;
    readonly forwarder: GDAv1Forwarder;

    constructor(
        hostAddress: string,
        gdaV1Address: string,
        gdaV1ForwarderAddress: string
    ) {
        super();
        this.host = new Host(hostAddress);
        this.contract = new ethers.Contract(
            gdaV1Address,
            IGeneralDistributionAgreementV1__factory.abi
        ) as IGeneralDistributionAgreementV1;
        this.forwarder = new ethers.Contract(
            gdaV1ForwarderAddress,
            GDAv1Forwarder__factory.abi
        ) as GDAv1Forwarder;
    }

    getNetFlow = async (params: GDAGetNetFlowParams): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAccount = normalizeAddress(params.account);
        return (
            await this.contract.getNetFlow(normalizedToken, normalizedAccount)
        ).toString();
    };

    getFlowRate = async (params: GDAGetFlowRateParams): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        return (
            await this.contract.getFlowRate(
                normalizedToken,
                normalizedFrom,
                normalizedPool
            )
        ).toString();
    };

    estimateFlowDistributionActualFlowRate = async (
        params: EstimateFlowDistributionActualFlowRateParams
    ): Promise<FlowDistributionActualFlowRateData> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        const data = await this.contract.estimateFlowDistributionActualFlowRate(
            normalizedToken,
            normalizedFrom,
            normalizedPool,
            params.requestedFlowRate
        );
        return {
            actualFlowRate: data.actualFlowRate.toString(),
            totalDistributionFlowRate:
                data.totalDistributionFlowRate.toString(),
        };
    };

    estimateDistributionActualAmount = async (
        params: EstimateDistributionActualAmountParams
    ): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        return (
            await this.contract.estimateDistributionActualAmount(
                normalizedToken,
                normalizedFrom,
                normalizedPool,
                params.requestedAmount
            )
        ).toString();
    };

    getPoolAdjustmentFlowRate = async (
        params: GetPoolAdjustmentFlowRateParams
    ): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedPool = normalizeAddress(params.pool);

        return (
            await this.contract.getPoolAdjustmentFlowRate(
                normalizedToken,
                normalizedPool
            )
        ).toString();
    };

    isPool = async (params: IsPoolParams): Promise<boolean> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAccount = normalizeAddress(params.account);

        return await this.contract.isPool(normalizedToken, normalizedAccount);
    };

    isMemberConnected = async (
        params: IsMemberConnectedParams
    ): Promise<boolean> => {
        const normalizedPool = normalizeAddress(params.pool);
        const normalizedMember = normalizeAddress(params.member);

        return await this.contract["isMemberConnected(address,address)"](
            normalizedPool,
            normalizedMember
        );
    };

    getPoolAdjustmentFlowInfo = async (
        poolAddress: string
    ): Promise<PoolAdjustmentFlowInfo> => {
        const normalizedPool = normalizeAddress(poolAddress);

        const data = await this.contract.getPoolAdjustmentFlowInfo(
            normalizedPool
        );
        return {
            recipient: data[0],
            flowHash: data[1],
            flowRate: data[2].toString(),
        };
    };

    createPool = async (params: CreatePoolParams) => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAdmin = normalizeAddress(params.admin);

        return this.contract.createPool(normalizedToken, normalizedAdmin);
    };

    connectPool = async (params: ConnectPoolParams) => {
        const normalizedPool = normalizeAddress(params.pool);
        const callData = gdaInterface.encodeFunctionData("connectPool", [
            normalizedPool,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData || "0x",
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.connectPool(
                normalizedPool,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    };

    disconnectPool = async (params: DisconnectPoolParams) => {
        const normalizedPool = normalizeAddress(params.pool);
        const callData = gdaInterface.encodeFunctionData("disconnectPool", [
            normalizedPool,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData || "0x",
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.disconnectPool(
                normalizedPool,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    };

    distribute = async (params: DistributeParams) => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        const callData = gdaInterface.encodeFunctionData("distribute", [
            normalizedToken,
            normalizedFrom,
            normalizedPool,
            params.requestedAmount,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData || "0x",
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.distribute(
                normalizedToken,
                normalizedFrom,
                normalizedPool,
                params.requestedAmount,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    };

    distributeFlow = async (params: DistributeFlowParams) => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        const callData = gdaInterface.encodeFunctionData("distributeFlow", [
            normalizedToken,
            normalizedFrom,
            normalizedPool,
            params.requestedFlowRate,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData || "0x",
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.distributeFlow(
                normalizedToken,
                normalizedFrom,
                normalizedPool,
                params.requestedFlowRate,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    };
}
