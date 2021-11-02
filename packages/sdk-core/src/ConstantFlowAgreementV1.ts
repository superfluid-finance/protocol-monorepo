import { ethers } from "ethers";
import {
    IAgreementV1Options,
    ICreateFlowParams,
    IDeleteFlowParams,
    IUpdateFlowParams,
} from "./interfaces";
import Operation from "./Operation";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { normalizeAddress } from "./utils";
import Host from "./Host";

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);

/**
 * @dev Constant Flow Agreement V1 Helper Class
 */
export default class ConstantFlowAgreementV1 {
    readonly options: IAgreementV1Options;
    readonly host: Host;

    constructor(options: IAgreementV1Options) {
        this.options = options;
        this.host = new Host(options.config.hostAddress);
    }

    /**
     * @dev Create a flow.
     * @param flowRate The specified flow rate.
     * @param receiver The receiver of the flow.
     * @param superToken The token to be flowed.
     * @param userData Extra user data provided.
     * @returns {Promise<Operation>} An instance of Operation which can be executed or batched.
     */
    createFlow = async ({
        flowRate,
        receiver,
        superToken,
        userData,
    }: ICreateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return await this.host.populateTransactionAndReturnOperation(
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
     * @returns {Promise<Operation>} An instance of Operation which can be executed or batched.
     */
    updateFlow = async ({
        flowRate,
        receiver,
        superToken,
        userData,
    }: IUpdateFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("updateFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);

        return await this.host.populateTransactionAndReturnOperation(
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
     * @returns {Promise<Operation>} An instance of Operation which can be executed or batched.
     */
    deleteFlow = async ({
        superToken,
        sender,
        receiver,
        userData,
    }: IDeleteFlowParams): Promise<Operation> => {
        const normalizedToken = normalizeAddress(superToken);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("deleteFlow", [
            normalizedToken,
            normalizedSender,
            normalizedReceiver,
            "0x",
        ]);

        return await this.host.populateTransactionAndReturnOperation(
            this.options.config.cfaV1Address,
            callData,
            userData
        );
    };
}
