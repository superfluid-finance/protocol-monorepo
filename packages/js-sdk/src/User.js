const {getErrorResponse} = require("./utils/error");

module.exports = class User {
    /**
     * @dev Create new Superfluid user object
     * @param {Framework} sf Superfluid framework object.
     * @param {string} address The EOA address of the user you want to create.
     * @param {string} token The address of the supertoken you want to interact with.
     */
    constructor({sf, address, token}) {
        this.sf = sf;
        this.address = address;
        this.token = token;
    }

    /**
     * @dev Returns instantiated details regarding the users' cfa and ida data.
     * @returns {object} top-level cfa property contains flows: Flow[] and netFlow: number
     * top-level ida property contains subscriptions: Subscription[]
     */
    async details() {
        try {
            const listFlows = this.sf.cfa.listFlows({
                superToken: this.token,
                account: this.address,
            });
            const getNewFlow = this.sf.cfa
                .getNetFlow({
                    superToken: this.token,
                    account: this.address,
                })
                .then((x) => x.toString());
            const listSubscriptions = this.sf.ida.listSubscriptions({
                superToken: this.token,
                subscriber: this.address,
            });
            const [flows, netFlow, subscriptions] = await Promise.all([
                listFlows,
                getNewFlow,
                listSubscriptions,
            ]);
            return {cfa: {flows, netFlow}, ida: {subscriptions}};
        } catch (e) {
            throw getErrorResponse(e, "user", "details");
        }
    }

    /**
     * @dev Allows you to create, update or delete a flow from the user you initialized.
     * @param {string} recipient the recipient of the flow agreement
     * @param {string} flowRate the agreed upon flowRate
     * @param {object} options options taken by cfa (userData, onTransaction, by (deleteFlow only))
     * @returns {Promise<Transaction | undefined>} web3 transaction object or undefined on error
     * NOTE: !0 in JS evaluates to true as 0 is a falsey value. We also stringify the flowRate,
     * just in case the user somehow is able to input a number (using JS).
     */
    async flow({recipient, flowRate, ...options}) {
        try {
            if (!recipient || flowRate == null || flowRate == undefined)
                throw "You must provide a recipient and flowRate";
            if (typeof flowRate !== "string")
                throw "You must provide flowRate as a string";
            const recipientAddress = recipient.address || recipient;
            if (flowRate === "0")
                return await this.sf.cfa.deleteFlow({
                    superToken: this.token,
                    sender: this.address,
                    receiver: recipientAddress,
                    ...options,
                });

            const existingFlow = await this.sf.cfa.getFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipientAddress,
            });
            if (existingFlow.flowRate !== "0")
                return await this.sf.cfa.updateFlow({
                    superToken: this.token,
                    sender: this.address,
                    receiver: recipientAddress,
                    flowRate,
                    ...options,
                });
            return await this.sf.cfa.createFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipientAddress,
                flowRate,
                ...options,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "flow");
        }
    }

    /**
     * @dev Create an Index using the IDA.
     * @param {number} poolId The id of the index.
     * @returns {Promise<Transaction | undefined>} web3 transaction object or undefined on error
     */
    async createPool({poolId: indexId}) {
        try {
            if (!indexId) throw "You must provide a poolId";
            const {exist} = await this.sf.ida.getIndex({
                superToken: this.token,
                publisher: this.address,
                indexId,
            });
            if (exist) throw "This pool has already been created";

            return await this.sf.ida.createIndex({
                superToken: this.token,
                publisher: this.address,
                indexId,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "createPool");
        }
    }

    /**
     * @dev Gives shares (units) to a recipient.
     * @param {string} recipient The recipient of the shares.
     * @param {number} shares The number of units the recipient will receive.
     * @param {number} poolId The id of the index.
     * @returns {Promise<Transaction | undefined>} web3 transaction object or undefined on error
     */
    async giveShares({recipient, shares, poolId: indexId}) {
        try {
            if (!recipient || !shares || !indexId)
                throw "You must provide a recipient, share amount, and poolId";
            const recipientAddress = recipient.address || recipient;

            const {exist} = await this.sf.ida.getIndex({
                superToken: this.token,
                publisher: this.address,
                indexId,
            });
            if (!exist) throw "This pool has not been created yet";

            return await this.sf.ida.updateSubscription({
                superToken: this.token,
                publisher: this.address,
                indexId,
                subscriber: recipientAddress,
                units: shares,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "giveShares");
        }
    }

    /**
     * @dev Distributes tokens to subscribers who are approved.
     * @param {number} poolId The id of the index.
     * @param {number} amount The amount of tokens to distribute.
     * @returns {Promise<Transaction | undefined>} web3 transaction object or undefined on error
     */
    async distributeToPool({poolId: indexId, amount}) {
        try {
            if (!indexId || !amount)
                throw "You must provide a poolId and amount";
            return await this.sf.ida.distribute({
                superToken: this.token,
                publisher: this.address,
                indexId,
                amount,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "distributeToPool");
        }
    }
};
