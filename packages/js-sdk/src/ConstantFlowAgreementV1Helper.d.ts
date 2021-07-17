export = ConstantFlowAgreementV1Helper;
declare class ConstantFlowAgreementV1Helper {
    static _sanitizeflowInfo({ timestamp, flowRate, deposit, owedDeposit }: {
        timestamp: any;
        flowRate: any;
        deposit: any;
        owedDeposit: any;
    }): {
        timestamp: Date;
        flowRate: any;
        deposit: any;
        owedDeposit: any;
    };
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf: any);
    _sf: any;
    _cfa: any;
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
    createFlow({ superToken, sender, receiver, flowRate, userData, onTransaction, }: any): Promise<any>;
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
    updateFlow({ superToken, sender, receiver, flowRate, userData, onTransaction, }: any): Promise<any>;
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
    deleteFlow({ superToken, sender, receiver, by, userData, onTransaction, }: any): Promise<any>;
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
    getFlow({ superToken, sender, receiver, }: any): Promise<object>;
    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    getNetFlow({ superToken, account, }: any): Promise<string>;
    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    getAccountFlowInfo({ superToken, account, }: any): Promise<string>;
    getFlowEvents({ token, receiver, sender }: {
        token: any;
        receiver?: any;
        sender?: any;
    }): Promise<any[]>;
    /**
     * @dev List flows of the account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<[]>}
     */
    listFlows({ superToken, account, onlyInFlows, onlyOutFlows, }: any): Promise<[]>;
}
