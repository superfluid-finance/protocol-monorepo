const autoBind = require("auto-bind");
const {completeTransaction} = require("./utils/general");

/**
 * @dev Constant flow agreement v1 helper class
 */
module.exports = class ConstantFlowAgreementV1Helper {
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf) {
        this._sf = sf;
        this._cfa = sf.agreements.cfa;
        autoBind(this);
    }

    /**
     * @dev Create a new flow
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {flowRateParam} flowRate the flowrate of the flow
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Object} gasOptions pass network gas parameters
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async createFlow({
        superToken,
        sender,
        receiver,
        flowRate,
        userData,
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const receiverNorm = await this._sf.utils.normalizeAddressParam(
            receiver
        );
        const flowRateNorm = this._sf.utils.normalizeFlowRateParam(flowRate);
        userData = userData || "0x";
        console.debug(
            `Create flow from ${sender} to ${receiver} at ${flowRate} for ${superToken} ...`
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._cfa.address,
                this._cfa.contract.methods
                    .createFlow(
                        superTokenNorm,
                        receiverNorm,
                        flowRateNorm,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: senderNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        this._sf._pushTxForGasReport(tx, "createFlow");
        console.debug("Flow created.");
        return tx;
    }

    /**
     * @dev Update a new flow with a new flow rate
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {flowRateParam} flowRate the flowrate of the flow
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Object} gasOptions pass network gas parameters
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async updateFlow({
        superToken,
        sender,
        receiver,
        flowRate,
        userData,
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const receiverNorm = await this._sf.utils.normalizeAddressParam(
            receiver
        );
        const flowRateNorm = this._sf.utils.normalizeFlowRateParam(flowRate);
        userData = userData || "0x";
        console.debug(
            `Update flow from ${sender} to ${receiver} to ${flowRate} for ${superToken} ...`
        );

        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._cfa.address,
                this._cfa.contract.methods
                    .updateFlow(
                        superTokenNorm,
                        receiverNorm,
                        flowRateNorm,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: senderNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });

        this._sf._pushTxForGasReport(tx, "updateFlow");
        console.debug("Flow updated.");
        return tx;
    }

    /**
     * @dev Delete a existing flow
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} sender sender of the flow
     * @param {addressParam} receiver receiver of the flow
     * @param {addressParam} by delete flow by a third party (liquidations)
     * @param {Buffer} userData the user data passed to the callbacks
     * @param {Object} gasOptions pass network gas parameters
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async deleteFlow({
        superToken,
        sender,
        receiver,
        by,
        userData,
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const receiverNorm = await this._sf.utils.normalizeAddressParam(
            receiver
        );
        const byNorm =
            (by && (await this._sf.utils.normalizeAddressParam(by))) ||
            senderNorm;
        userData = userData || "0x";
        console.debug(
            `Delete flow from ${sender} to ${receiver} by ${
                by || byNorm
            } for ${superToken} ...`
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._cfa.address,
                this._cfa.contract.methods
                    .deleteFlow(superTokenNorm, senderNorm, receiverNorm, "0x")
                    .encodeABI(),
                userData,
            ],
            sender: byNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        this._sf._pushTxForGasReport(tx, "deleteFlow");
        console.debug("Flow deleted.");
        return tx;
    }

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
    async getFlow({
        superToken,
        sender,
        receiver,
        //unit
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const receiverNorm = await this._sf.utils.normalizeAddressParam(
            receiver
        );
        const result = await this._cfa.getFlow(
            superTokenNorm,
            senderNorm,
            receiverNorm
        );
        return this.constructor._sanitizeflowInfo(result);
    }

    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    async getNetFlow({
        superToken,
        account,
        //unit
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const accountNorm = await this._sf.utils.normalizeAddressParam(account);
        const netFlow = await this._cfa.getNetFlow(superTokenNorm, accountNorm);
        return netFlow.toString();
    }

    /**
     * @dev Get information of the net flow of an account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<string>} Net flow rate of the account
     */
    async getAccountFlowInfo({
        superToken,
        account,
        //unit
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const accountNorm = await this._sf.utils.normalizeAddressParam(account);
        const result = await this._cfa.getAccountFlowInfo(
            superTokenNorm,
            accountNorm
        );
        return this.constructor._sanitizeflowInfo(result);
    }

    static _sanitizeflowInfo({timestamp, flowRate, deposit, owedDeposit}) {
        return {
            timestamp: new Date(Number(timestamp.toString()) * 1000),
            flowRate: flowRate.toString(),
            deposit: deposit.toString(),
            owedDeposit: owedDeposit.toString(),
        };
    }

    async getFlowEvents({token, receiver = null, sender = null}) {
        let flows = await this._sf.getPastEvents(this._cfa, "FlowUpdated", {
            token,
            receiver,
            sender,
        });
        return Object.values(
            flows.reduce((acc, i) => {
                acc[i.sender + ":" + i.receiver] = i;
                return acc;
            }, {})
        ).filter((i) => i.flowRate.toString() != "0");
    }

    /**
     * @dev List flows of the account
     * @param {tokenParam} superToken superToken for the flow
     * @param {addressParam} account the account for the query
     * @return {Promise<[]>}
     */
    async listFlows({
        superToken,
        account,
        onlyInFlows,
        onlyOutFlows,
        //unit
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const accountNorm = await this._sf.utils.normalizeAddressParam(account);
        const result = {};
        if (!onlyOutFlows) {
            result.inFlows = (
                await this.getFlowEvents({
                    receiver: accountNorm,
                    token: superTokenNorm,
                })
            ).map((f) => ({
                sender: f.sender,
                receiver: f.receiver,
                flowRate: f.flowRate.toString(),
            }));
        }
        if (!onlyInFlows) {
            result.outFlows = (
                await this.getFlowEvents({
                    token: superTokenNorm,
                    sender: accountNorm,
                })
            ).map((f) => ({
                sender: f.sender,
                receiver: f.receiver,
                flowRate: f.flowRate.toString(),
            }));
        }
        return result;
    }
};
