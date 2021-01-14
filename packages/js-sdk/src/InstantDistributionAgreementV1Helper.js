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
        console.debug("Index updated.");
        return tx;
    }

    async updateSupscription({
        superToken,
        indexId,
        subscriber,
        sender,
        units,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .updateSubscription(
                        superToken,
                        indexId,
                        subscriber,
                        units,
                        "0x"
                    )
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Subscription updated.");
        return tx;
    }

    async approveSupscription({
        superToken,
        indexId,
        publisher,
        sender,
        units,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .approveSubscription(superToken, publisher, indexId, "0x")
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Subscription approved.");
        return tx;
    }

    async deleteSupscription({
        superToken,
        indexId,
        publisher,
        subscriber,
        sender,
        units,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .deleteSubscription(
                        superToken,
                        publisher,
                        indexId,
                        subscriber,
                        "0x"
                    )
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Subscription deleted.");
        return tx;
    }
};
