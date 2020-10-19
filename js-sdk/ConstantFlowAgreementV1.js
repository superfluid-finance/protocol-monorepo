const {
    normalizeTokenParam,
    normalizeAddressParam,
    normalizeFlowRateParam,
} = require("./utils");

/**
 * @dev Constant flow agreement v1 helper class
 */
function ConstantFlowAgreementV1(sf) {
    this._sf = sf;
    this._cfa = sf.agreements.cfa;
}

ConstantFlowAgreementV1.prototype.createFlow = async function ({
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
};

ConstantFlowAgreementV1.prototype.updateFlow = async function () {

};

ConstantFlowAgreementV1.prototype.deleteFlow = async function () {

};

ConstantFlowAgreementV1.prototype.getFlow = async function () {

};

ConstantFlowAgreementV1.prototype.getNetFlow = async function ({
    superToken,
    account,
    //unit
}) {
    const superTokenNorm = await normalizeTokenParam(superToken);
    const accountNorm = await normalizeAddressParam(account);
    return await this._cfa.getNetFlow(superTokenNorm, accountNorm);
};

ConstantFlowAgreementV1.prototype.listFlows = async function () {

};

module.exports = ConstantFlowAgreementV1;
