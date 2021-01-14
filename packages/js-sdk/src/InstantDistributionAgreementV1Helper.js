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

    async distribute({
        superToken,
        indexId,
        amount,
        sender,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .distribute(superToken, indexId, amount, "0x")
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Distribution complete.");
        return tx;
    }

    async claim({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
        userData = "0x",
        onTransaction = () => null
    }) {
        const tx = await this._sf.host
            .callAgreement(
                this._ida.address,
                this._ida.contract.methods
                    .claim(superToken, publisher, indexId, subscriber, "0x")
                    .encodeABI(),
                userData,
                { from: sender }
            )
            .on("transactionHash", onTransaction);
        console.debug("Claim complete.");
        return tx;
    }

    async getIndex({ superToken, publisher, indexId }) {
        const result = await this._ida.getIndex(superToken, publisher, indexId);
        return this.constructor._sanitizeIndexInfo(result);
    }

    async listSubscriptions({ superToken, subscriber }) {
        const result = await this._ida.listSubscriptions(
            superToken,
            subscriber
        );
        return this.constructor._sanitizeSubscriptionInfo(result);
    }

    static _sanitizeIndexInfo({
        exist,
        indexValue,
        totalUnitsApproved,
        totalUnitsPending
    }) {
        return {
            exist,
            indexValue: indexValue.toString(),
            totalUnitsApproved: totalUnitsApproved.toString(),
            totalUnitsPending: totalUnitsPending.toString()
        };
    }

    static _sanitizeSubscriptionInfo({ publishers, indexIds, unitsList }) {
        return {
            publishers,
            indexIds: indexIds.map(id => id.toString()),
            unitsList: unitsList.map(units => units.toString())
        };
    }
};
