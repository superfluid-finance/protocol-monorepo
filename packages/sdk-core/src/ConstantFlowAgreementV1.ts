import { ethers } from "ethers";
import {
    IAgreementV1Options,
    ICreateFlowParams,
    IDeleteFlowParams,
    IUpdateFlowParams,
} from "./interfaces";
import Operation from "./Operation";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { getSanitizedTimestamp, normalizeAddress } from "./utils";
import Host from "./Host";
import { IConstantFlowAgreementV1 } from "./typechain";

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

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
            IConstantFlowAgreementV1ABI
        ) as IConstantFlowAgreementV1;
    }

    // CFA Read Functions

    getFlow = async ({
        superToken,
        sender,
        receiver,
        providerOrSigner,
    }: {
        superToken: string;
        sender: string;
        receiver: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }) => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);
        const flowData = await this.cfaContract
            .connect(providerOrSigner)
            .getFlow(normalizedToken, normalizedSender, normalizedReceiver);
        return this._sanitizeflowInfo(flowData);
    };

    getAccountFlowInfo = async ({
        superToken,
        account,
        providerOrSigner,
    }: {
        superToken: string;
        account: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }) => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedAccount = normalizeAddress(account);
        const flowData = await this.cfaContract
            .connect(providerOrSigner)
            .getAccountFlowInfo(normalizedToken, normalizedAccount);
        return this._sanitizeflowInfo(flowData);
    };

    getNetFlow = async ({
        superToken,
        account,
        providerOrSigner,
    }: {
        superToken: string;
        account: string;
        providerOrSigner: ethers.providers.Provider | ethers.Signer;
    }) => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedAccount = normalizeAddress(account);
        return (
            await this.cfaContract
                .connect(providerOrSigner)
                .getNetFlow(normalizedToken, normalizedAccount)
        ).toString();
    };

    // CFA Write Functions

    /**
     * @dev Create a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    createFlow = ({
        flowRate,
        receiver,
        superToken,
        userData,
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
            userData
        );
    };

    /**
     * @dev Update a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    updateFlow = ({
        flowRate,
        receiver,
        superToken,
        userData,
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
            userData
        );
    };

    /**
     * @dev Delete a flow.
     * @param superToken The token to be flowed.
     * @param sender The sender of the flow.
     * @param receiver The receiver of the flow.
     * @param userData Extra user data provided.
     * @returns {Operation} An instance of Operation which can be executed or batched.
     */
    deleteFlow = ({
        superToken,
        sender,
        receiver,
        userData,
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
            userData
        );
    };

    _sanitizeflowInfo({
        timestamp,
        flowRate,
        deposit,
        owedDeposit,
    }: {
        timestamp: ethers.BigNumber;
        flowRate: ethers.BigNumber;
        deposit: ethers.BigNumber;
        owedDeposit: ethers.BigNumber;
    }) {
        return {
            timestamp: getSanitizedTimestamp(timestamp),
            flowRate: flowRate.toString(),
            deposit: deposit.toString(),
            owedDeposit: owedDeposit.toString(),
        };
    }
}
