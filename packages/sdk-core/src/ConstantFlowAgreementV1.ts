import { ethers } from "ethers";

import Host from "./Host";
import Operation from "./Operation";
import { SFError } from "./SFError";
import SuperfluidAgreement from "./SuperfluidAgreement";
import {
    FlowRateAllowanceParams,
    FlowRateAllowanceWithPermissionsParams,
    ICreateFlowByOperatorParams,
    ICreateFlowParams,
    IDeleteFlowParams,
    IFullControlParams,
    IGetAccountFlowInfoParams,
    IGetFlowOperatorDataByIDParams,
    IGetFlowOperatorDataParams,
    IGetFlowParams,
    IUpdateFlowByOperatorParams,
    IUpdateFlowOperatorPermissionsParams,
    IUpdateFlowParams,
    IWeb3FlowInfo,
    IWeb3FlowInfoParams,
    IWeb3FlowOperatorData,
    IWeb3FlowOperatorDataParams,
} from "./interfaces";
import {
    CFAv1Forwarder,
    CFAv1Forwarder__factory,
    IConstantFlowAgreementV1,
    IConstantFlowAgreementV1__factory,
} from "./typechain-types";
import {
    getSanitizedTimestamp,
    isPermissionsClean,
    normalizeAddress,
} from "./utils";

const cfaInterface = IConstantFlowAgreementV1__factory.createInterface();

/**
 * Constant Flow Agreement V1 Helper Class
 * @description A helper class to interact with the CFAV1 contract.
 */
export default class ConstantFlowAgreementV1 extends SuperfluidAgreement {
    readonly host: Host;
    readonly contract: IConstantFlowAgreementV1;
    readonly forwarder: CFAv1Forwarder;

    constructor(
        hostAddress: string,
        cfaV1Address: string,
        cfaV1ForwarderAddress: string
    ) {
        super();
        this.host = new Host(hostAddress);
        this.contract = new ethers.Contract(
            cfaV1Address,
            IConstantFlowAgreementV1__factory.abi
        ) as IConstantFlowAgreementV1;
        this.forwarder = new ethers.Contract(
            cfaV1ForwarderAddress,
            CFAv1Forwarder__factory.abi
        ) as CFAv1Forwarder;
    }

    /** ### CFA Read Functions ### */

    /**
     * Get the details of a flow.
     * @param superToken the superToken of the agreement
     * @param sender the sender of the flow
     * @param receiver the receiver of the flow
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getFlow = async (params: IGetFlowParams): Promise<IWeb3FlowInfo> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);
        try {
            const flowData = await this.contract
                .connect(params.providerOrSigner)
                .getFlow(normalizedToken, normalizedSender, normalizedReceiver);
            return this._sanitizeFlowInfo(flowData);
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                message: "There was an error getting the flow",
                cause: err,
            });
        }
    };

    /**
     * Get the flow info of an account (net flow).
     * @param superToken the superToken of the agreement
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getAccountFlowInfo = async (
        params: IGetAccountFlowInfoParams
    ): Promise<IWeb3FlowInfo> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedAccount = normalizeAddress(params.account);
        try {
            const flowData = await this.contract
                .connect(params.providerOrSigner)
                .getAccountFlowInfo(normalizedToken, normalizedAccount);
            return this._sanitizeFlowInfo(flowData);
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                message:
                    "There was an error getting the account flow information",
                cause: err,
            });
        }
    };

    /**
     * Get the net flow of an account.
     * @param superToken the superToken of the agreement
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<string>} Web3 Flow info object
     */
    getNetFlow = async (params: IGetAccountFlowInfoParams): Promise<string> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedAccount = normalizeAddress(params.account);
        try {
            return (
                await this.contract
                    .connect(params.providerOrSigner)
                    .getNetFlow(normalizedToken, normalizedAccount)
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                message: "There was an error getting net flow",
                cause: err,
            });
        }
    };

    /**
     * Get flow operator data.
     * @param superToken the superToken of the agreement
     * @param sender the sender
     * @param flowOperator the flowOperator
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowOperatorData>} Web3 Flow info object
     */
    getFlowOperatorData = async (
        params: IGetFlowOperatorDataParams
    ): Promise<IWeb3FlowOperatorData> => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        try {
            const flowOperatorData = await this.contract
                .connect(params.providerOrSigner)
                .getFlowOperatorData(
                    normalizedToken,
                    normalizedSender,
                    normalizedFlowOperator
                );
            return this._sanitizeFlowOperatorData(flowOperatorData);
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                message: "There was an error getting flow operator data",
                cause: err,
            });
        }
    };

    /**
     * Get flow operator data using the flowOperatorId.
     * @param superToken the superToken of the agreement
     * @param flowOperatorId The keccak256 hash of encoded string "flowOperator", sender and flowOperator
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowOperatorData>} Web3 Flow info object
     */
    getFlowOperatorDataByID = async (
        params: IGetFlowOperatorDataByIDParams
    ): Promise<IWeb3FlowOperatorData> => {
        const normalizedToken = normalizeAddress(params.superToken);
        try {
            const flowOperatorData = await this.contract
                .connect(params.providerOrSigner)
                .getFlowOperatorDataByID(
                    normalizedToken,
                    params.flowOperatorId
                );
            return this._sanitizeFlowOperatorData({
                ...flowOperatorData,
                flowOperatorId: params.flowOperatorId,
            });
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                message: "There was an error getting flow operator data",
                cause: err,
            });
        }
    };

    /** ### CFA Write Functions ### */

    /**
     * Create a flow.
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = (params: ICreateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const normalizedSender = normalizeAddress(params.sender);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            params.flowRate,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.createFlow(
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                params.flowRate,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /**
     * Update a flow.
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = (params: IUpdateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const normalizedSender = normalizeAddress(params.sender);

        const callData = cfaInterface.encodeFunctionData("updateFlow", [
            normalizedToken,
            normalizedReceiver,
            params.flowRate,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.updateFlow(
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                params.flowRate,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /**
     * Delete a flow.
     * @param superToken The token to be flowed.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = (params: IDeleteFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);

        const callData = cfaInterface.encodeFunctionData("deleteFlow", [
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            "0x",
        ]);

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.deleteFlow(
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                params.userData || "0x",
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /** ### CFA ACL Write Functions (byOperator) ### */

    /**
     * Increase the flow rate allowance for an ACL operator.
     * @param superToken The token to be flowed.
     * @param flowOperator The operator of the flow.
     * @param flowRateAllowanceDelta The amount to increase the flow rate allowance by.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    increaseFlowRateAllowance(params: FlowRateAllowanceParams): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        const callData = cfaInterface.encodeFunctionData(
            "increaseFlowRateAllowance",
            [
                normalizedToken,
                normalizedFlowOperator,
                params.flowRateAllowanceDelta,
                "0x",
            ]
        );
        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    }

    /**
     * Decrease the flow rate allowance for an ACL operator.
     * @param superToken The token to be flowed.
     * @param flowOperator The operator of the flow.
     * @param flowRateAllowanceDelta The amount to decrease the flow rate allowance by.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    decreaseFlowRateAllowance(params: FlowRateAllowanceParams): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        const callData = cfaInterface.encodeFunctionData(
            "decreaseFlowRateAllowance",
            [
                normalizedToken,
                normalizedFlowOperator,
                params.flowRateAllowanceDelta,
                "0x",
            ]
        );
        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    }

    /**
     * Increase the flow rate allowance and sets permissions for an ACL operator.
     * @param superToken The token to be flowed.
     * @param flowOperator The operator of the flow.
     * @param permissionsDelta The permissions to be add for the operator.
     * @param flowRateAllowanceDelta The amount to increase the flow rate allowance by.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    increaseFlowRateAllowanceWithPermissions(
        params: FlowRateAllowanceWithPermissionsParams
    ): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        if (!isPermissionsClean(params.permissionsDelta)) {
            throw new SFError({
                type: "UNCLEAN_PERMISSIONS",
                message: "The desired permissions are unclean",
            });
        }

        const callData = cfaInterface.encodeFunctionData(
            "increaseFlowRateAllowanceWithPermissions",
            [
                normalizedToken,
                normalizedFlowOperator,
                params.permissionsDelta,
                params.flowRateAllowanceDelta,
                "0x",
            ]
        );
        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    }

    /**
     * Decrease the flow rate allowance and sets permissions for an ACL operator.
     * @param superToken The token to be flowed.
     * @param flowOperator The operator of the flow.
     * @param permissionsDelta The permissions to be remove from the operator.
     * @param flowRateAllowanceDelta The amount to decrease the flow rate allowance by.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    decreaseFlowRateAllowanceWithPermissions(
        params: FlowRateAllowanceWithPermissionsParams
    ): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        if (!isPermissionsClean(params.permissionsDelta)) {
            throw new SFError({
                type: "UNCLEAN_PERMISSIONS",
                message: "The desired permissions are unclean",
            });
        }

        const callData = cfaInterface.encodeFunctionData(
            "decreaseFlowRateAllowanceWithPermissions",
            [
                normalizedToken,
                normalizedFlowOperator,
                params.permissionsDelta,
                params.flowRateAllowanceDelta,
                "0x",
            ]
        );
        return this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );
    }

    /**
     * Update permissions for a flow operator as a sender.
     * @param superToken The token to be flowed.
     * @param flowOperator The permission grantee address
     * @param permission The permissions to set.
     * @param flowRateAllowance The flowRateAllowance granted to the flow operator.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlowOperatorPermissions(
        params: IUpdateFlowOperatorPermissionsParams
    ): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);
        if (!isPermissionsClean(params.permissions)) {
            throw new SFError({
                type: "UNCLEAN_PERMISSIONS",
                message: "The desired permissions are unclean",
            });
        }

        if (Number(params.flowRateAllowance) < 0) {
            throw new SFError({
                type: "NEGATIVE_FLOW_ALLOWANCE",
                message: "No negative flow allowance allowed",
            });
        }

        const callData = cfaInterface.encodeFunctionData(
            "updateFlowOperatorPermissions",
            [
                normalizedToken,
                normalizedFlowOperator,
                params.permissions,
                params.flowRateAllowance,
                "0x",
            ]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.updateFlowOperatorPermissions(
                normalizedToken,
                normalizedFlowOperator,
                params.permissions,
                params.flowRateAllowance,
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    }

    /**
     * Give flow operator full control - max flow rate and create/update/delete permissions.
     * @param superToken The token to be flowed.
     * @param flowOperator The permission grantee address
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     */
    authorizeFlowOperatorWithFullControl(
        params: IFullControlParams
    ): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);

        const callData = cfaInterface.encodeFunctionData(
            "authorizeFlowOperatorWithFullControl",
            [normalizedToken, normalizedFlowOperator, "0x"]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.grantPermissions(
                normalizedToken,
                normalizedFlowOperator,
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    }

    /**
     * Revoke flow operator control - set flow rate to 0 with no permissions.
     * @param superToken The token to be flowed.
     * @param flowOperator The permission grantee address
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     */
    revokeFlowOperatorWithFullControl(params: IFullControlParams): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);

        const callData = cfaInterface.encodeFunctionData(
            "revokeFlowOperatorWithFullControl",
            [normalizedToken, normalizedFlowOperator, "0x"]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const forwarderPopulatedTxnPromise =
            this.forwarder.populateTransaction.revokePermissions(
                normalizedToken,
                normalizedFlowOperator,
                params.overrides || {}
            );

        return this._getCallAgreementOperation(
            callAgreementOperation,
            forwarderPopulatedTxnPromise,
            params.shouldUseCallAgreement
        );
    }

    /**
     * Create a flow as an operator
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlowByOperator = (params: ICreateFlowByOperatorParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const normalizedSender = normalizeAddress(params.sender);

        const callData = cfaInterface.encodeFunctionData(
            "createFlowByOperator",
            [
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                params.flowRate,
                "0x",
            ]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const createFlowOperation = this.createFlow(params);

        return this._getCallAgreementOperation(
            callAgreementOperation,
            createFlowOperation.forwarderPopulatedPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /**
     * Update a flow as an operator.
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlowByOperator = (params: IUpdateFlowByOperatorParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);

        const callData = cfaInterface.encodeFunctionData(
            "updateFlowByOperator",
            [
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                params.flowRate,
                "0x",
            ]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const updateFlowOperation = this.updateFlow(params);

        return this._getCallAgreementOperation(
            callAgreementOperation,
            updateFlowOperation.forwarderPopulatedPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /**
     * Delete a flow as an operator.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlowByOperator = (params: IDeleteFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);

        const callData = cfaInterface.encodeFunctionData(
            "deleteFlowByOperator",
            [normalizedToken, normalizedSender, normalizedReceiver, "0x"]
        );

        const callAgreementOperation = this.host.callAgreement(
            this.contract.address,
            callData,
            params.userData,
            params.overrides
        );

        const deleteFlowOperation = this.deleteFlow(params);

        return this._getCallAgreementOperation(
            callAgreementOperation,
            deleteFlowOperation.forwarderPopulatedPromise,
            params.shouldUseCallAgreement || normalizedSender === ""
        );
    };

    /** ### Internal Helper Functions ### */

    /**
     * Sanitizes flow info, converting BigNumber to string.
     * @param timestamp last updated timestamp of flow
     * @param flowRate the current flow rate
     * @param deposit the deposit amount
     * @param owedDeposit any owed deposit
     * @returns {IWeb3FlowInfo} sanitized web3 flow info
     */
    _sanitizeFlowInfo = (params: IWeb3FlowInfoParams): IWeb3FlowInfo => {
        return {
            timestamp: getSanitizedTimestamp(params.timestamp),
            flowRate: params.flowRate.toString(),
            deposit: params.deposit.toString(),
            owedDeposit: params.owedDeposit.toString(),
        };
    };

    /**
     * Sanitizes flow operator data, converting BigNumber to string.
     * @param flowOperatorId The keccak256 hash of encoded string "flowOperator", sender and flowOperator
     * @param permissions the permissions
     * @param flowRateAllowance the flow rate allowance granted to the flow operator
     * @returns {IWeb3FlowOperatorData} sanitized web3 flow info
     */
    _sanitizeFlowOperatorData = (
        params: IWeb3FlowOperatorDataParams
    ): IWeb3FlowOperatorData => {
        return {
            flowOperatorId: params.flowOperatorId,
            permissions: params.permissions.toString(),
            flowRateAllowance: params.flowRateAllowance.toString(),
        };
    };
}
