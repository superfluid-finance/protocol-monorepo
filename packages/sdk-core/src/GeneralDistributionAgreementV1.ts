import { ethers } from "ethers";

import Host from "./Host";
import { SFError } from "./SFError";
import SuperfluidAgreement from "./SuperfluidAgreement";
import SuperfluidPoolClass from "./SuperfluidPool";
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
    GetPoolAdjustmentFlowInfoParams,
    GetPoolAdjustmentFlowRateParams,
    IsMemberConnectedParams,
    IsPoolParams,
    PoolAdjustmentFlowInfo,
} from "./interfaces";
import {
    GDAv1Forwarder,
    GDAv1Forwarder__factory,
    IGeneralDistributionAgreementV1,
    IGeneralDistributionAgreementV1__factory,
} from "./typechain-types";
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

    /**
     * Retrieves the net flow for a specific token and account.
     *
     * @param token The token address.
     * @param account The account address.
     * @param providerOrSigner A provider or signer object
     * @returns The net flow of the account for the token.
     */
    getNetFlow = async (params: GDAGetNetFlowParams): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAccount = normalizeAddress(params.account);
        try {
            return (
                await this.contract
                    .connect(params.providerOrSigner)
                    .getNetFlow(normalizedToken, normalizedAccount)
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message: "There was an error getting the GDA net flow.",
                cause: err,
            });
        }
    };

    /**
     * Retrieves the flow rate for a specific token, sender, and pool.
     *
     * @param token The token address.
     * @param from The sender address.
     * @param pool The pool address.
     * @param providerOrSigner A provider or signer object
     * @returns The flow rate from the sender to the pool for the token.
     */
    getFlowRate = async (params: GDAGetFlowRateParams): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        try {
            return (
                await this.contract
                    .connect(params.providerOrSigner)
                    .getFlowRate(
                        normalizedToken,
                        normalizedFrom,
                        normalizedPool
                    )
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message: "There was an error getting the GDA flow rate.",
                cause: err,
            });
        }
    };

    /**
     * Estimates the flow distribution's actual flow rate for a specific token, sender, and pool.
     *
     * @param token The token address.
     * @param from The sender address.
     * @param pool The pool address.
     * @param requestedFlowRate The requested flow rate.
     * @param providerOrSigner A provider or signer object
     * @returns The flow distribution's actual flow rate and the total distribution flow rate for the pool.
     */
    estimateFlowDistributionActualFlowRate = async (
        params: EstimateFlowDistributionActualFlowRateParams
    ): Promise<FlowDistributionActualFlowRateData> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);

        try {
            const data = await this.contract
                .connect(params.providerOrSigner)
                .estimateFlowDistributionActualFlowRate(
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
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error estimating the GDA flow distribution's actual flow rate.",
                cause: err,
            });
        }
    };

    /**
     * Estimates the distribution's actual amount for a specific token, sender, and pool.
     *
     * @param token The token address.
     * @param from The sender address.
     * @param pool The pool address.
     * @param requestedAmount The requested amount.
     * @param providerOrSigner A provider or signer object
     * @returns The actual amount that will be distributed.
     */
    estimateDistributionActualAmount = async (
        params: EstimateDistributionActualAmountParams
    ): Promise<string> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedPool = normalizeAddress(params.pool);
        try {
            return (
                await this.contract
                    .connect(params.providerOrSigner)
                    .estimateDistributionActualAmount(
                        normalizedToken,
                        normalizedFrom,
                        normalizedPool,
                        params.requestedAmount
                    )
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error estimating the GDA distribution's actual amount.",
                cause: err,
            });
        }
    };

    /**
     * Retrieves the pool adjustment flow rate for a specific token and pool.
     *
     * @param token The token address.
     * @param pool The pool address.
     * @param providerOrSigner A provider or signer object
     * @returns The pool adjustment flow rate for the token and pool.
     */
    getPoolAdjustmentFlowRate = async (
        params: GetPoolAdjustmentFlowRateParams
    ): Promise<string> => {
        const normalizedPool = normalizeAddress(params.pool);

        try {
            return (
                await this.contract
                    .connect(params.providerOrSigner)
                    .getPoolAdjustmentFlowRate(normalizedPool)
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error getting the GDA pool adjustment flow rate.",
                cause: err,
            });
        }
    };

    /**
     * Checks if a given token and account form a pool.
     *
     * @param token The token address.
     * @param account The account address.
     * @param providerOrSigner A provider or signer object
     * @returns Whether the account is a pool for the token.
     */
    isPool = async (params: IsPoolParams): Promise<boolean> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAccount = normalizeAddress(params.account);

        try {
            return await this.contract
                .connect(params.providerOrSigner)
                .isPool(normalizedToken, normalizedAccount);
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error checking if the account is a pool.",
                cause: err,
            });
        }
    };

    /**
     * Checks if a member is connected to a specific pool.
     *
     * @param pool The pool address.
     * @param member The member address.
     * @param providerOrSigner A provider or signer object
     * @returns Whether the member is connected to the pool.
     */
    isMemberConnected = async (
        params: IsMemberConnectedParams
    ): Promise<boolean> => {
        const normalizedPool = normalizeAddress(params.pool);
        const normalizedMember = normalizeAddress(params.member);

        try {
            return await this.contract
                .connect(params.providerOrSigner)
                .isMemberConnected(normalizedPool, normalizedMember);
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error checking if the member is connected to the pool.",
                cause: err,
            });
        }
    };

    /**
     * Retrieves the pool adjustment flow information for a specific pool.
     *
     * @param pool The address of the pool.
     * @param providerOrSigner A provider or signer object
     * @returns The recipient of the pool adjustment flow, the flow hash and the rate of the adjustment flow.
     */
    getPoolAdjustmentFlowInfo = async (
        params: GetPoolAdjustmentFlowInfoParams
    ): Promise<PoolAdjustmentFlowInfo> => {
        const normalizedPool = normalizeAddress(params.pool);

        try {
            const data = await this.contract
                .connect(params.providerOrSigner)
                .getPoolAdjustmentFlowInfo(normalizedPool);
            return {
                recipient: data[0],
                flowHash: data[1],
                flowRate: data[2].toString(),
            };
        } catch (err) {
            throw new SFError({
                type: "GDAV1_READ",
                message:
                    "There was an error getting the GDA pool adjustment flow information.",
                cause: err,
            });
        }
    };

    /**
     * Creates a new pool with the given token and admin.
     *
     * @param token The token address.
     * @param admin The admin address.
     * @returns CreatePoolTxn and SuperfluidPool instance
     */
    createPool = async (
        params: CreatePoolParams
    ): Promise<{
        createPoolTxn: ethers.ContractTransaction;
        pool: SuperfluidPoolClass;
    }> => {
        const normalizedToken = normalizeAddress(params.token);
        const normalizedAdmin = normalizeAddress(params.admin);

        try {
            const createPoolTxn = await this.contract
                .connect(params.signer)
                .createPool(normalizedToken, normalizedAdmin, params.config);
            const txnReceipt = await createPoolTxn.wait();
            const poolCreatedEvent = txnReceipt.events?.find(
                (x) => x.event === "PoolCreated"
            );
            const poolAddress =
                poolCreatedEvent?.args?.pool || ethers.constants.AddressZero;
            return {
                createPoolTxn,
                pool: new SuperfluidPoolClass(poolAddress),
            };
        } catch (err) {
            throw new SFError({
                type: "GDAV1_WRITE",
                message: "There was an error creating the GDA pool.",
                cause: err,
            });
        }
    };

    /**
     * Connects a pool to the contract.
     *
     * @param pool The pool address.
     * @param userData The user data.
     * @param overrides The transaction overrides.
     * @returns The call agreement operation result.
     */
    connectPool = (params: ConnectPoolParams) => {
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

    /**
     * Disconnects a pool from the contract.
     *
     * @param pool The pool address.
     * @param userData The user data.
     * @param overrides The transaction overrides.
     * @returns The call agreement operation result.
     */
    disconnectPool = (params: DisconnectPoolParams) => {
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

    /**
     * Distributes funds from the sender's account to the specified pool.
     *
     * @param token The token address.
     * @param from The sender's address.
     * @param pool The pool address.
     * @param requestedAmount The requested amount to distribute.
     * @param userData The user data.
     * @param overrides The transaction overrides.
     * @returns The call agreement operation result.
     */
    distribute = (params: DistributeParams) => {
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

    /**
     * Distributes the flow from the sender's account to the specified pool.
     *
     * @param token The token address.
     * @param from The sender's address.
     * @param pool The pool address.
     * @param requestedFlowRate The requested flow rate.
     * @param userData The user data.
     * @param overrides The transaction overrides.
     * @returns The call agreement operation result.
     */
    distributeFlow = (params: DistributeFlowParams) => {
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
