import { ethers } from "ethers";

import Host from "./Host";
import Operation from "./Operation";
import { SFError } from "./SFError";
import IConstantFlowAgreementV1ABI from "./abi/IConstantFlowAgreementV1.json";
import {
    IAgreementV1Options,
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
} from "./typechain";
import {
    getSanitizedTimestamp,
    isPermissionsClean,
    normalizeAddress,
} from "./utils";

const cfaInterface = new ethers.utils.Interface(
    IConstantFlowAgreementV1ABI.abi
);
/**
 * Constant Flow Agreement V1 Helper Class
 * @description A helper class to interact with the CFAV1 contract.
 */
export default class ConstantFlowAgreementV1 {
    readonly options: IAgreementV1Options;
    readonly host: Host;
    readonly forwarder: CFAv1Forwarder;
    readonly contract: IConstantFlowAgreementV1;

    constructor(options: IAgreementV1Options) {
        this.options = options;
        this.host = new Host(options.config.hostAddress);
        this.forwarder = new ethers.Contract(
            this.options.config.cfaV1ForwarderAddress,
            CFAv1Forwarder__factory.abi
        ) as CFAv1Forwarder;
        this.contract = new ethers.Contract(
            this.options.config.cfaV1Address,
            IConstantFlowAgreementV1ABI.abi
        ) as IConstantFlowAgreementV1;
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

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData("createFlow", [
                normalizedToken,
                normalizedReceiver,
                params.flowRate,
                "0x",
            ]);

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn = this.forwarder.populateTransaction.createFlow(
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            params.flowRate,
            params.userData ?? "0x"
        );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    };

    /**
     * Update a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = (params: IUpdateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const normalizedSender = normalizeAddress(params.sender);

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData("updateFlow", [
                normalizedToken,
                normalizedReceiver,
                params.flowRate,
                "0x",
            ]);

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn = this.forwarder.populateTransaction.updateFlow(
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            params.flowRate,
            params.userData ?? "0x"
        );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    };

    /**
     * Delete a flow.
     * @param superToken The token to be flowed.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = (params: IDeleteFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedSender = normalizeAddress(params.sender);
        const normalizedReceiver = normalizeAddress(params.receiver);

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData("deleteFlow", [
                normalizedToken,
                normalizedSender,
                normalizedReceiver,
                "0x",
            ]);

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn = this.forwarder.populateTransaction.deleteFlow(
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            params.userData ?? "0x"
        );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    };

    /** ### CFA ACL Write Functions (byOperator) ### */

    /**
     * Update permissions for a flow operator as a sender.
     * @param superToken The token to be flowed.
     * @param flowOperator The permission grantee address
     * @param permission The permissions to set.
     * @param flowRateAllowance The flowRateAllowance granted to the flow operator.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
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

        if (params.shouldUseCallAgreement) {
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
            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn =
            this.forwarder.populateTransaction.updateFlowOperatorPermissions(
                normalizedToken,
                normalizedFlowOperator,
                params.permissions,
                params.flowRateAllowance
            );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    }

    /**
     * Give flow operator full control - max flow rate and create/update/delete permissions.
     * @param superToken The token to be flowed.
     * @param flowOperator The permission grantee address
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
     */
    authorizeFlowOperatorWithFullControl(
        params: IFullControlParams
    ): Operation {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedFlowOperator = normalizeAddress(params.flowOperator);

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData(
                "authorizeFlowOperatorWithFullControl",
                [normalizedToken, normalizedFlowOperator, "0x"]
            );
            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn = this.forwarder.populateTransaction.grantPermissions(
            normalizedToken,
            normalizedFlowOperator
        );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
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

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData(
                "revokeFlowOperatorWithFullControl",
                [normalizedToken, normalizedFlowOperator, "0x"]
            );

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        const txn = this.forwarder.populateTransaction.revokePermissions(
            normalizedToken,
            normalizedFlowOperator
        );

        return new Operation(txn, "SUPERFLUID_CALL_AGREEMENT");
    }

    /**
     * Create a flow as an operator
     * @param flowRate The specified flow rate.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @param shouldUseCallAgreement Whether to use callAgreement, or the CFAv1Forwarder
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlowByOperator = (params: ICreateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(params.superToken);
        const normalizedReceiver = normalizeAddress(params.receiver);
        const normalizedSender = normalizeAddress(params.sender);

        if (params.shouldUseCallAgreement) {
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

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        return this.createFlow(params);
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

        if (params.shouldUseCallAgreement) {
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

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        return this.updateFlow(params);
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

        if (params.shouldUseCallAgreement) {
            const callData = cfaInterface.encodeFunctionData(
                "deleteFlowByOperator",
                [normalizedToken, normalizedSender, normalizedReceiver, "0x"]
            );

            return this.host.callAgreement(
                this.options.config.cfaV1Address,
                callData,
                params.userData,
                params.overrides
            );
        }

        return this.deleteFlow(params);
    };

    /** ### Private Functions ### */

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
