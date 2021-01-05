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
            const netFlow = await this.sf.cfa.getNetFlow({ superToken: this.token, account: this.address });
            return { cfa: { flows, netFlow } };
        } catch (e) {
            throw getErrorResponse(e, "user", "details");
        }
    }

    async flow({recipient, flowRate}) {
        try {
            if (flowRate === 0) {
                return await this.sf.cfa.deleteFlow({});
            }
            const existingFlow = await this.sf.cfa.getFlow({ superToken: this.token, sender: this.address, receiver: recipient });
            if (existingFlow > 0) {
                return await this.sf.cfa.updateFlow({});
            }
            return await this.sf.cfa.createFlow({
                superToken: this.token,
                sender: this.address,
                receiver: recipient,
                flowRate
                // userData,
                // onTransaction = () => null,
            });
        } catch (e) {
            throw getErrorResponse(e, "user", "flow");
        }
    }
};
