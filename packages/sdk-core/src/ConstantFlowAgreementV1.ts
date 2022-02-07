import { ethers } from "ethers";

import Host from "./Host";
import Operation from "./Operation";
import { SFError } from "./SFError";
import IConstantFlowAgreementV1ABI from "./abi/IConstantFlowAgreementV1.json";
import {
    IAgreementV1Options,
    ICreateFlowParams,
    IDeleteFlowParams,
    IGetAccountFlowInfoParams,
    IGetFlowParams,
    IUpdateFlowParams,
    IWeb3FlowInfo,
    IWeb3FlowInfoParams,
} from "./interfaces";
import { IConstantFlowAgreementV1 } from "./typechain";
import { getSanitizedTimestamp, normalizeAddress } from "./utils";

const cfaInterface = new ethers.utils.Interface(
    IConstantFlowAgreementV1ABI.abi
);

/**
 * @dev Constant Flow Agreement V1 Helper Class
 * @description A helper class to interact with the CFAV1 contract.
 */
export default class ConstantFlowAgreementV1 {
    readonly options: IAgreementV1Options;
    readonly host: Host;

    constructor(options: IAgreementV1Options) {
        this.options = options;
        this.host = new Host(options.config.hostAddress);
    }

    private get cfaContract() {
        return new ethers.Contract(
            this.options.config.cfaV1Address,
            IConstantFlowAgreementV1ABI.abi
        ) as IConstantFlowAgreementV1;
    }

    // CFA Read Functions

    /**
     * @dev Get the details of a flow.
     * @param superToken the superToken of the agreement
     * @param sender the sender of the flow
     * @param receiver the receiver of the flow
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getFlow = async ({
        superToken,
        sender,
        receiver,
        providerOrSigner,
    }: IGetFlowParams): Promise<IWeb3FlowInfo> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);
        try {
            const flowData = await this.cfaContract
                .connect(providerOrSigner)
                .getFlow(normalizedToken, normalizedSender, normalizedReceiver);
            return this._sanitizeflowInfo(flowData);
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                customMessage: "There was an error getting the flow",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Get the flow info of an account (net flow).
     * @param superToken the superToken of the agreement
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<IWeb3FlowInfo>} Web3 Flow info object
     */
    getAccountFlowInfo = async ({
        superToken,
        account,
        providerOrSigner,
    }: IGetAccountFlowInfoParams): Promise<IWeb3FlowInfo> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedAccount = normalizeAddress(account);
        try {
            const flowData = await this.cfaContract
                .connect(providerOrSigner)
                .getAccountFlowInfo(normalizedToken, normalizedAccount);
            return this._sanitizeflowInfo(flowData);
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                customMessage:
                    "There was an error getting the account flow information",
                errorObject: err,
            });
        }
    };

    /**
     * @dev Get the net flow of an account.
     * @param superToken the superToken of the agreement
     * @param account the account we're querying
     * @param providerOrSigner a provider or signer object
     * @returns {Promise<string>} Web3 Flow info object
     */
    getNetFlow = async ({
        superToken,
        account,
        providerOrSigner,
    }: IGetAccountFlowInfoParams): Promise<string> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedAccount = normalizeAddress(account);
        try {
            return (
                await this.cfaContract
                    .connect(providerOrSigner)
                    .getNetFlow(normalizedToken, normalizedAccount)
            ).toString();
        } catch (err) {
            throw new SFError({
                type: "CFAV1_READ",
                customMessage: "There was an error getting net flow",
                errorObject: err,
            });
        }
    };

    // CFA Write Functions

    /**
     * @dev Create a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = ({
        flowRate,
        receiver,
        superToken,
        userData,
        overrides,
    }: ICreateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.cfaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Update a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = ({
        flowRate,
        receiver,
        superToken,
        userData,
        overrides,
    }: IUpdateFlowParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("updateFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.cfaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Delete a flow.
     * @param superToken The token to be flowed.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @param overrides ethers overrides object for more control over the transaction sent.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = ({
        superToken,
        sender,
        receiver,
        userData,
        overrides,
    }: IDeleteFlowParams): Operation => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("deleteFlow", [
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            "0x",
        ]);

        return this.host.populateCallAgreementTxnAndReturnOperation(
            this.options.config.cfaV1Address,
            callData,
            userData,
            overrides
        );
    };

    /**
     * @dev Sanitizes flow info, converting BigNumber to string.
     * @param timestamp last updated timestamp of flow
     * @param flowRate the current flow rate
     * @param deposit the deposit amount
     * @param owedDeposit any owed depsit
     * @returns {IWeb3FlowInfo} sanitized web3 flow info
     */
    _sanitizeflowInfo = ({
        timestamp,
        flowRate,
        deposit,
        owedDeposit,
    }: IWeb3FlowInfoParams): IWeb3FlowInfo => {
        return {
            timestamp: getSanitizedTimestamp(timestamp),
            flowRate: flowRate.toString(),
            deposit: deposit.toString(),
            owedDeposit: owedDeposit.toString(),
        };
    };
}
