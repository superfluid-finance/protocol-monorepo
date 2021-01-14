/**
 * @dev Instant distribution agreement v1 helper class
 */
module.exports = class InstantDistributionAgreementV1Helper {
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf) {
        this._sf = sf;
        this._ida = sf.agreements.ida;
    }

    async createIndex({
        superToken,
        indexId,
        sender,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .createIndex(superToken, indexId, "0x")
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Index created.");
        return tx;
    }

    async updateIndex({
        superToken,
        indexId,
        indexValue,
        sender,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .updateIndex(superToken, indexId, indexValue, "0x")
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Index created.");
        return tx;
    }
};
