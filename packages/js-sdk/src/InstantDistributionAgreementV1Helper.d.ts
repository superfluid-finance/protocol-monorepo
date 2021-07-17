export = InstantDistributionAgreementV1Helper;
declare class InstantDistributionAgreementV1Helper {
    static _sanitizeIndexData({ exist, indexValue, totalUnitsApproved, totalUnitsPending, }: {
        exist: any;
        indexValue: any;
        totalUnitsApproved: any;
        totalUnitsPending: any;
    }): {
        exist: any;
        indexValue: any;
        totalUnitsApproved: any;
        totalUnitsPending: any;
    };
    static _sanitizeSubscriptionData({ exist, approved, units, pendingDistribution, }: {
        exist: any;
        approved: any;
        units: any;
        pendingDistribution: any;
    }): {
        exist: any;
        approved: any;
        units: any;
        pendingDistribution: any;
    };
    static _sanitizeSubscriptionInfo({ publishers, indexIds, unitsList }: {
        publishers: any;
        indexIds: any;
        unitsList: any;
    }): any;
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf: any);
    _sf: any;
    _ida: any;
    /**
     * @dev Create a new index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    createIndex({ superToken, publisher, indexId, userData, onTransaction, }: any): Promise<any>;
    /**
     * @dev Distribute tokens to an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {BN} amount Amount to be distributed
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    distribute({ superToken, publisher, indexId, amount, userData, onTransaction, }: any): Promise<any>;
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
    updateIndex({ superToken, publisher, indexId, indexValue, userData, onTransaction, }: any): Promise<any>;
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
    updateSubscription({ superToken, publisher, indexId, subscriber, units, userData, onTransaction, }: any): Promise<any>;
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
    approveSubscription({ superToken, publisher, indexId, subscriber, userData, onTransaction, }: any): Promise<any>;
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
    revokeSubscription({ superToken, indexId, publisher, subscriber, userData, onTransaction, }: any): Promise<any>;
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
    deleteSubscription({ superToken, indexId, publisher, subscriber, sender, userData, onTransaction, }: any): Promise<any>;
    /**
     * @dev Get details of a subscription
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getSubscription({ superToken, publisher, indexId, subscriber }: any): Promise<any>;
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
    claim({ superToken, publisher, indexId, subscriber, sender, userData, onTransaction, }: any): Promise<any>;
    /**
     * @dev Get details of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getIndex({ superToken, publisher, indexId }: any): Promise<any>;
    /**
     * @dev List indices of a publisher
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listIndices({ superToken, publisher }: any): Promise<any>;
    /**
     * @dev List subscribers of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubcribers({ superToken, publisher, indexId }: any): Promise<any>;
    /**
     * @dev List subscriptions of an account
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubscriptions({ superToken, subscriber }: any): Promise<any>;
}
