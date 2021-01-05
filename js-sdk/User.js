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
            const flows = await listFlows({
                superToken: this.token,
                account: this.address
            });
            const netFlow = await getNetFlow({ superToken: this.token, account: this.address });
            return { cfa: { flows, netFlow } };
        } catch (e) {
            throw getErrorResponse(e, "user", "details");
        }
    }

    async flow(recipient, flowRate) {
        try {
            if (flowRate === 0) {
                await this.cfa.deleteFlow({});
                return;
            }
            const existingFlow = await getFlow({ superToken: this.token, sender: this.address, receiver: recipient });
            if (existingFlow > 0) {
                await this.cfa.updateFlow({});
                return;
            }
            await this.cfa.createFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipient,
                flowRate
                // userData,
                // onTransaction = () => null,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "createFlow");
        }
    }
    async getFlow(recipient) {
        try {
        } catch (e) {
            throw getErrorResponse(e, "user", "getFlow");
        }
    }
};
