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
            return { cfa: { flows, netFlow } };
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
};
