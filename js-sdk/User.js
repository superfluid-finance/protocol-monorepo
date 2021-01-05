const { getErrorResponse } = require("./utils/error");

export default class User {
    constructor({ sf, address, token, options }) {
        this.sf = sf;
        this.address = address;
        this.token = token;
        this.options = options;
    }

    async details() {
        const flows = await listFlows({
            superToken: this.token,
            account: this.address
        });
        const netFlow = await getNetFlow({ superToken: this.token, account: this.address });
        return { cfa: { flows, netFlow } };
    }
    async sendFlow(recipient, flowRate) {
        try {
            // await async createFlow({
            //     superToken,
            //     sender,
            //     receiver,
            //     flowRate,
            //     userData,
            //     onTransaction = () => null,
            // })
            // return interestSent;
        } catch (error) {
            throw getErrorResponse(error, "user", "interestSent");
        }
    }
}
