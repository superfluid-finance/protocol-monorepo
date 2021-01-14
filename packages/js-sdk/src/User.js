const { getErrorResponse } = require("./utils/error");

module.exports = class User {
    constructor({ sf, address, token, options }) {
        this.sf = sf;
        this.address = address;
        this.token = token;
        this.options = options;
    }

    async details() {
        try {
            const flows = await this.sf.cfa.listFlows({
                superToken: this.token,
                account: this.address
            });
            const netFlow = (
                await this.sf.cfa.getNetFlow({
                    superToken: this.token,
                    account: this.address
                })
            ).toString();
            const subscriptions = await this.sf.ida.listSubscriptions({
                superToken: this.token,
                subscriber: this.address
            });
            return { cfa: { flows, netFlow }, ida: { subscriptions } };
        } catch (e) {
            throw getErrorResponse(e, "user", "details");
        }
    }

    async flow({ recipient, flowRate, ...options }) {
        try {
            if (!recipient || !flowRate)
                throw "You must provide a recipient and flowRate";
            const recipientAddress = recipient.address || recipient;
            if (flowRate === "0")
                return await this.sf.cfa.deleteFlow({
                    superToken: this.token,
                    sender: this.address,
                    receiver: recipientAddress,
                    ...options
                });

            const existingFlow = await this.sf.cfa.getFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipientAddress
            });
            if (existingFlow.flowRate !== "0")
                return await this.sf.cfa.updateFlow({
                    superToken: this.token,
                    sender: this.address,
                    receiver: recipientAddress,
                    flowRate,
                    ...options
                });
            return await this.sf.cfa.createFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipientAddress,
                flowRate,
                ...options
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "flow");
        }
    }

    async createPool({ poolId: indexId }) {
        try {
            if (!indexId) throw "You must provide a poolId";
            const { exist } = await this.sf.ida.getIndex({
                superToken: this.token,
                publisher: this.address,
                indexId
            });
            if (exist) throw "This pool has already been created";

            return await this.sf.ida.createIndex({
                superToken: this.token,
                indexId,
                sender: this.address
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "createPool");
        }
    }

    async giveShares({ recipient, shares, poolId: indexId }) {
        try {
            if (!recipient || !shares || !indexId)
                throw "You must provide a recipient, share amount, and poolId";
            const recipientAddress = recipient.address || recipient;

            const { exist } = await this.sf.ida.getIndex({
                superToken: this.token,
                publisher: this.address,
                indexId
            });
            if (!exist) throw "This pool has not been created yet";

            return await this.sf.ida.updateSupscription({
                superToken: this.token,
                indexId,
                subscriber: recipientAddress,
                units: shares,
                sender: this.address
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "giveShares");
        }
    }

    async distributeToPool({ poolId: indexId, amount }) {
        try {
            if (!indexId || !amount)
                throw "You must provide a poolId and amount";
            await this.sf.ida.distribute({
                superToken: this.token,
                indexId,
                amount,
                sender: this.address
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "distributeToPool");
        }
    }
};
