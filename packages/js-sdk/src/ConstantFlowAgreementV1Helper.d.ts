import { Transaction } from "web3-core";
import Framework from "./Framework";
import type { LoadedContract } from "./loadContracts";
import type BN from 'bn.js';

export = ConstantFlowAgreementV1Helper;
declare class ConstantFlowAgreementV1Helper {
    static _sanitizeflowInfo({ timestamp, flowRate, deposit, owedDeposit }: {
        timestamp: any;
        flowRate: any;
        deposit: any;
        owedDeposit: any;
    }): {
        timestamp: Date;
        flowRate: string;
        deposit: string;
        owedDeposit: string;
    };
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf: Framework);
    _sf: Framework;
    _cfa: LoadedContract;
    /**
     * @dev Create a new flow
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {flowRateParam} flowRate the flowrate of the flow
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    createFlow({ superToken, sender, receiver, flowRate, userData, onTransaction, }: {
        superToken: string;
        sender: string;
        receiver: string;
        flowRate: BN | string;
        userData: any;
        onTransaction: () => void;
    }): Promise<Transaction>;
    /**
     * @dev Update a new flow with a new flow rate
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {flowRateParam} flowRate the flowrate of the flow
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    updateFlow({ superToken, sender, receiver, flowRate, userData, onTransaction, }: {
        superToken: string;
        sender: string;
        receiver: string;
        flowRate: BN | string;
        userData: any;
        onTransaction: () => void;
    }): Promise<Transaction>;
    /**
     * @dev Delete a existing flow
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {addressParam} by delete flow by a third party (liquidations)
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    deleteFlow({ superToken, sender, receiver, by, userData, onTransaction, }: {
        superToken: string;
        sender: string;
        receiver: string;
        by: string;
        userData: any;
        onTransaction: () => void;
    }): Promise<Transaction>;
    /**
     * @dev Get information of a existing flow
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @return {Promise<object>} Informationo about the flow:
     *         - <Date> timestamp, time when the flow was last updated
     *         - <string> flowRate, flow rate of the flow
     *         - <string> deposit, deposit of the flow
     *         - <string> owedDeposit, owed deposit of the flow
     */
    getFlow({ superToken, sender, receiver, }: {
        superToken: string;
        sender: string;
        receiver: string;
    }): Promise<{
        timestamp: Date;
        flowRate: string;
        deposit: string;
        owedDeposit: string;
    }>;
    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    getNetFlow({ superToken, account, }: {
        superToken: string;
        account: string;
    }): Promise<string>;
    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    getAccountFlowInfo({ superToken, account, }: {
        superToken: string;
        account: string;
    }): Promise<string>;

    getFlowEvents({ token, receiver, sender }: {
        token: string;
        receiver?: string;
        sender?: string;
    }): Promise<string[]>;
    /**
     * @dev List flows of the account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<[]>}
     */
    listFlows({ superToken, account, onlyInFlows, onlyOutFlows, }: {
        superToken: string;
        account: string;
        onlyInFlows: boolean;
        onlyOutFlows: boolean;
    }): Promise<{
        inFlows?: {
            sender: string;
            receiver: string;
            flowRate: string;
        };
        outFlows?: {
            sender: string;
            receiver: string;
            flowRate: string;
        }
    }>;
}
