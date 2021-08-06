import { Transaction } from "web3-core";
import Framework from "./Framework";
import type { LoadedContract } from "./loadContracts";
import type BN from 'bn.js';

export interface Subscription {
    exist: boolean;
    approved: boolean;
    units: BN;
    pendingDistribution: BN;
}

export declare class InstantDistributionAgreementV1Helper {
    static _sanitizeIndexData({ exist, indexValue, totalUnitsApproved, totalUnitsPending, }: {
        exist: boolean;
        indexValue: any;
        totalUnitsApproved: any;
        totalUnitsPending: any;
    }): {
        exist: boolean;
        indexValue: string;
        totalUnitsApproved: string;
        totalUnitsPending: string;
    };
    static _sanitizeSubscriptionData({ exist, approved, units, pendingDistribution, }: {
        exist: boolean;
        approved: boolean;
        units: any;
        pendingDistribution: any;
    }): {
        exist: boolean;
        approved: boolean;
        units: string;
        pendingDistribution: string;
    };
    static _sanitizeSubscriptionInfo({ publishers, indexIds, unitsList }: {
        publishers: any[];
        indexIds: any;
        unitsList: any;
    }): {
       publisher: string;
       indexId: string;
       units: string;
    }[];
    /**
     * @dev Create new helper class
     * @param {Framework} sf Superfluid Framework object
     *
     * NOTE: You should first call async function Framework.initialize to initialize the object.
     */
    constructor(sf: Framework);
    _sf: Framework;
    _ida: LoadedContract;
    /**
     * @dev Create a new index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    createIndex({ superToken, publisher, indexId, userData, onTransaction, }: {
        superToken: string;
        publisher: string;
        indexId: number;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
    /**
     * @dev Distribute tokens to an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {BN} amount Amount to be distributed
     * @param {Function} onTransaction function to be called when transaction hash has been generated
     * @return {Promise<Transaction>} web3 transaction object
     */
    distribute({ superToken, publisher, indexId, amount, userData, onTransaction, }: {
        superToken: string;
        publisher: string;
        indexId: number;
        amount: BN;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
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
    updateIndex({ superToken, publisher, indexId, indexValue, userData, onTransaction, }: {
        superToken: string; 
        publisher: string;
        indexId: number; 
        indexValue: BN;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
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
    updateSubscription({ superToken, publisher, indexId, subscriber, units, userData, onTransaction, }: {
        superToken: string; 
        publisher: string; 
        indexId: number;
        subscriber: string;
        units: BN;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
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
    approveSubscription({ superToken, publisher, indexId, subscriber, userData, onTransaction, }: {
        superToken: string;
        publisher: string;
        indexId: number;
        subscriber: string;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
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
    revokeSubscription({ superToken, indexId, publisher, subscriber, userData, onTransaction, }: {
        superToken: string;
        indexId: number;
        publisher: string;
        subscriber: string;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
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
    deleteSubscription({ superToken, indexId, publisher, subscriber, sender, userData, onTransaction, }: {
        superToken: string;
        indexId: number;
        publisher: string;
        subscriber: string;
        sender: string;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
    /**
     * @dev Get details of a subscription
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getSubscription({ superToken, publisher, indexId, subscriber }: {
        superToken: string;
        publisher: string;
        indexId: number;
        subscriber: string;
    }): Promise<Subscription>;
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
    claim({ superToken, publisher, indexId, subscriber, sender, userData, onTransaction, }: {
        superToken: string;
        publisher: string;
        indexId: number;
        subscriber: string;
        sender: string;
        userData: any;
        onTransaction: () => any;
    }): Promise<Transaction>;
    /**
     * @dev Get details of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getIndex({ superToken, publisher, indexId }: {
        superToken: string;
        publisher: string;
        indexId: number;
    }): Promise<Subscription>;
    /**
     * @dev List indices of a publisher
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listIndices({ superToken, publisher }: {
        superToken: string;
        publisher: string;
    }): Promise<Subscription>;
    /**
     * @dev List subscribers of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubcribers({ superToken, publisher, indexId }: {
        superToken: string;
        publisher: string;
        indexId: number;
    }): Promise<Subscription>;
    /**
     * @dev List subscriptions of an account
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubscriptions({ superToken, subscriber }: {
        superToken: string;
        subscriber: string;
    }): Promise<Subscription[]>;
}
