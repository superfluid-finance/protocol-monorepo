const autoBind = require("auto-bind");
const {completeTransaction} = require("./utils/general");

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
        autoBind(this);
    }

    /**
     * @dev Create a new index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async createIndex({
        superToken,
        publisher,
        indexId,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .createIndex(superTokenNorm, indexId, "0x")
                    .encodeABI(),
                userData,
            ],
            sender: publisherNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Index created.");
        return tx;
    }

    /**
     * @dev Distribute tokens to an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {BN} amount Amount to be distributed
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async distribute({
        superToken,
        publisher,
        indexId,
        amount,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .distribute(superTokenNorm, indexId, amount, "0x")
                    .encodeABI(),
                userData,
            ],
            sender: publisherNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Distribution complete.");
        return tx;
    }

    /**
     * @dev Update the value of a index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     *
     * NOTE:
     * it has the same effect as doing distribute, but closer to the low level data structure
     * of the index.
     */
    async updateIndex({
        superToken,
        publisher,
        indexId,
        indexValue,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .updateIndex(superTokenNorm, indexId, indexValue, "0x")
                    .encodeABI(),
                userData,
            ],
            sender: publisherNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Index updated.");
        return tx;
    }

    /**
     * @dev Update number of units of a subscription by the publisher of the index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @param {BN} units Units of the subscription
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    async updateSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
        units,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .updateSubscription(
                        superTokenNorm,
                        indexId,
                        subscriberNorm,
                        units,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: publisherNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Subscription updated.");
        return tx;
    }

    /**
     * @dev Approve the subscription by a subscriber of the index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     *
     * NOTE:
     * By approving, the subscriber can use the balance the moment the publishder distributes
     * tokens without doing the extra claim step.
     */
    async approveSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .approveSubscription(
                        superTokenNorm,
                        publisherNorm,
                        indexId,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: subscriberNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Subscription approved.");
        return tx;
    }

    /**
     * @dev Revoke the subscription by a subscriber of the index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     *
     * NOTE:
     * By revoking, the subscriber will need to do claim step in order to get the tokens.
     */
    async revokeSubscription({
        superToken,
        indexId,
        publisher,
        subscriber,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .revokeSubscription(
                        superTokenNorm,
                        publisherNorm,
                        indexId,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: subscriberNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Subscription revoked.");
        return tx;
    }

    /**
     * @dev Delete the subscription by the publisher or a subscriber of the index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @param {addressParam} sender Publisher or subscriber of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     *
     * NOTE:
     * It means both revoking and clear the units of a subscription.
     */
    async deleteSubscription({
        superToken,
        indexId,
        publisher,
        subscriber,
        sender,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .deleteSubscription(
                        superTokenNorm,
                        publisherNorm,
                        indexId,
                        subscriberNorm,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: senderNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Subscription deleted.");
        return tx;
    }

    /**
     * @dev Get details of a subscription
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @return {Promise<Subscription>} Subscription data
     */
    async getSubscription({superToken, publisher, indexId, subscriber}) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const result = await this._ida.getSubscription.call(
            superTokenNorm,
            publisherNorm,
            indexId,
            subscriberNorm
        );
        return this.constructor._sanitizeSubscriptionData(result);
    }

    /**
     * @dev Claim distributions to a subscriber of the index by anyone.
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @param {addressParam} sender Any account to claim the distribution for the subscriber
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     *
     * NOTE:
     * If the subscriber has not approved the subscription, anyone can claim the distribution for him.
     */
    async claim({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
        userData = "0x",
        gasOptions = {},
        onTransaction = () => null,
    }) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const senderNorm = await this._sf.utils.normalizeAddressParam(sender);
        const tx = await completeTransaction({
            sf: this._sf,
            args: [
                this._ida.address,
                this._ida.contract.methods
                    .claim(
                        superTokenNorm,
                        publisherNorm,
                        indexId,
                        subscriberNorm,
                        "0x"
                    )
                    .encodeABI(),
                userData,
            ],
            sender: senderNorm,
            method: this._sf.host.callAgreement,
            gasOptions: {
                maxPriorityFeePerGas: gasOptions.maxPriorityFeePerGas,
                maxFeePerGas: gasOptions.maxFeePerGas,
            },
            onTransaction,
        });
        console.debug("Claim complete.");
        return tx;
    }

    /**
     * @dev Get details of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    async getIndex({superToken, publisher, indexId}) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        const result = await this._ida.getIndex(
            superTokenNorm,
            publisherNorm,
            indexId
        );
        return this.constructor._sanitizeIndexData(result);
    }

    /**
     * @dev List indices of a publisher
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @return {Promise<Subscription>} Subscription data
     */
    async listIndices({superToken, publisher}) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        return (
            await this._sf.getPastEvents(this._ida, "IndexCreated", {
                token: superTokenNorm,
                publisher: publisherNorm,
            })
        ).map((e) => Number(e.indexId.toString()));
    }

    /**
     * @dev List subscribers of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    async listSubcribers({superToken, publisher, indexId}) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const publisherNorm = await this._sf.utils.normalizeAddressParam(
            publisher
        );
        let updates;
        updates = await this._sf.getPastEvents(this._ida, "IndexUnitsUpdated", {
            token: superTokenNorm,
            publisher: publisherNorm,
            indexId,
        });
        return Object.values(
            updates.reduce((acc, i) => {
                acc[i.subscriber] = i;
                return acc;
            }, {})
        )
            .filter((i) => i.units.toString() != "0")
            .map((i) => ({
                subscriber: i.subscriber,
                units: i.units.toString(),
            }));
    }

    /**
     * @dev List subscriptions of an account
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    async listSubscriptions({superToken, subscriber}) {
        const superTokenNorm = await this._sf.utils.normalizeTokenParam(
            superToken
        );
        const subscriberNorm = await this._sf.utils.normalizeAddressParam(
            subscriber
        );
        const result = await this._ida.listSubscriptions(
            superTokenNorm,
            subscriberNorm
        );
        return this.constructor._sanitizeSubscriptionInfo(result);
    }

    static _sanitizeIndexData({
        exist,
        indexValue,
        totalUnitsApproved,
        totalUnitsPending,
    }) {
        return {
            exist,
            indexValue: indexValue.toString(),
            totalUnitsApproved: totalUnitsApproved.toString(),
            totalUnitsPending: totalUnitsPending.toString(),
        };
    }

    static _sanitizeSubscriptionData({
        exist,
        approved,
        units,
        pendingDistribution,
    }) {
        return {
            exist,
            approved,
            units: units.toString(),
            pendingDistribution: pendingDistribution.toString(),
        };
    }

    static _sanitizeSubscriptionInfo({publishers, indexIds, unitsList}) {
        return publishers.map((publisher, i) => ({
            publisher,
            indexId: Number(indexIds[i].toString()),
            units: unitsList[i].toString(),
        }));
    }
};
