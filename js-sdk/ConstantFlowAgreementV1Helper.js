const {
    normalizeTokenParam,
    normalizeAddressParam,
    normalizeFlowRateParam,
} = require("./utils");

/**
 * @dev Constant flow agreement v1 helper class
 */
module.exports = class ConstantFlowAgreementV1Helper {

    constructor(sf) {
        this._sf = sf;
        this._cfa = sf.agreements.cfa;
    }

    async createFlow({
        superToken,
        sender,
        receiver,
        flowRate
    }) {
        const superTokenNorm = await normalizeTokenParam(superToken);
        const receiverNorm = await normalizeAddressParam(receiver);
        const flowRateNorm = normalizeFlowRateParam(flowRate);
        console.debug(`Creating flow from ${sender} to ${receiver} at a flow rate of ${flowRate} ...`);
        await this._sf.host.callAgreement(
            this._cfa.address,
            this._cfa.contract.methods.createFlow(
                superTokenNorm,
                receiverNorm,
                flowRateNorm,
                "0x"
            ).encodeABI(),
            {
                from: sender,
            }
        );
        console.debug("Flow created.");
    }

    async updateFlow() {
    }

    async deleteFlow() {
    }

    async getFlow() {
    }

    async getNetFlow({
        superToken,
        account,
        //unit
    }) {
        const superTokenNorm = await normalizeTokenParam(superToken);
        const accountNorm = await normalizeAddressParam(account);
        return await this._cfa.getNetFlow(superTokenNorm, accountNorm);
    }

    async listFlows() {

    }

};
