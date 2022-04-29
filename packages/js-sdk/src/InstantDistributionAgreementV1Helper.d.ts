import type { Transaction } from "web3-core";
import type { Framework } from "./Framework";
import type { LoadedContract } from "./loadContracts";
import type BN from 'bn.js';
import { GasOptions } from "./types/gasOptions";

// comes from getIndex in IDAv1 contract,
// then passed into _sanitizeIndexData
// then returned from getIndex
export interface IndexData {
    exist: boolean;
    indexValue: string;
    totalUnitsApproved: string;
    totalUnitsPending: string;
}

// comes from getSubscription in IDAv1 contract,
// then passed into _sanitizeSubscriptionData
// then returned from getSubscription
export interface SubscriptionData {
    exist: boolean;
    approved: boolean;
    units: string;
    pendingDistribution: string
}

// comes from listSubscriptions in IDAv1 contract,
// then passed into __sanitizeSubscriptionInfo
// then returned from listSubscriptions
export type SubscriptionInfoList = {
    publisher: string;
    indexId: number;
    units: string;
}[]

// returned from listSubscribers after mapping
export type SubscriberList = {
    subscriber: string;
    units: string;
}[]

// method option params
// useful for projects wrapping the SDK
export interface CreateIndexOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface DistributeOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    amount: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface UpdateIndexOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    indexValue: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface UpdateSubscriptionOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    subscriber: string;
    units: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface ApproveSubscriptionOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    subscriber: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface RevokeSubscriptionOptions {
    superToken: string;
    indexId: number;
    publisher: string;
    subscriber: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface DeleteSubscriptionOptions {
    superToken: string;
    indexId: number;
    publisher: string;
    subscriber: string;
    sender: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface GetSubscriptionOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    subscriber: string;
}

export interface ClaimOptions {
    superToken: string;
    publisher: string;
    indexId: number;
    subscriber: string;
    sender: string;
    userData?: string;
    onTransaction?: () => any;
    gasOptions?: GasOptions;
}

export interface GetIndexOptions {
    superToken: string;
    publisher: string;
    indexId: number;
}

export interface ListIndicesOptions {
    superToken: string;
    publisher: string;
}

export interface ListSubcribersOptions {
    superToken: string;
    publisher: string;
    indexId: number;
}

export interface ListSubscriptionsOptions {
    superToken: string;
    subscriber: string;
}

export declare class InstantDistributionAgreementV1Helper {
    static _sanitizeIndexData({ exist, indexValue, totalUnitsApproved, totalUnitsPending, }: {
        exist: boolean;
        indexValue: number | BN;
        totalUnitsApproved: number | BN;
        totalUnitsPending: number | BN;
    }): IndexData;
    static _sanitizeSubscriptionData({ exist, approved, units, pendingDistribution, }: {
        exist: boolean;
        approved: boolean;
        units: number | BN;
        pendingDistribution: number | BN;
    }): SubscriptionData;
    static _sanitizeSubscriptionInfo({ publishers, indexIds, unitsList }: {
        publishers: any[];
        indexIds: number; // the contract returns uint32, the SDK wraps it in Number(),
        unitsList: number[] | BN[];
    }): SubscriptionInfoList;
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
     * @param {GasOptions} gasOptions pass network gas parameters
     * @return {Promise<Transaction>} web3 transaction object
     */
    createIndex({
        superToken,
        publisher,
        indexId,
        userData,
        onTransaction,
        gasOptions?: GasOptions,
    }: CreateIndexOptions): Promise<Transaction>;
/**
 * @dev Distribute tokens to an index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {BN} amount Amount to be distributed
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @return {Promise<Transaction>} web3 transaction object
 * @param {GasOptions} gasOptions pass network gas parameters
 */
    distribute({
        superToken,
        publisher,
        indexId,
        amount,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: DistributeOptions): Promise<Transaction>;
/**
 * @dev Update the value of a index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 *
 * NOTE:
 * it has the same effect as doing distribute, but closer to the low level data structure
 * of the index.
 */
    updateIndex({
        superToken,
        publisher,
        indexId,
        indexValue,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: UpdateIndexOptions): Promise<Transaction>;
/**
 * @dev Update number of units of a subscription by the publisher of the index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {addressParam} subscriber Subscriber of the index
 * @param {BN} units Units of the subscription
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 */
    updateSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
        units,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: UpdateSubscriptionOptions): Promise<Transaction>;
/**
 * @dev Approve the subscription by a subscriber of the index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {addressParam} subscriber Subscriber of the index
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 *
 * NOTE:
 * By approving, the subscriber can use the balance the moment the publishder distributes
 * tokens without doing the extra claim step.
 */
    approveSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: ApproveSubscriptionOptions): Promise<Transaction>;
/**
 * @dev Revoke the subscription by a subscriber of the index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {addressParam} subscriber Subscriber of the index
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 *
 * NOTE:
 * By revoking, the subscriber will need to do claim step in order to get the tokens.
 */
    revokeSubscription({
        superToken,
        indexId,
        publisher,
        subscriber,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: RevokeSubscriptionOptions): Promise<Transaction>;
/**
 * @dev Delete the subscription by the publisher or a subscriber of the index
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {addressParam} subscriber Subscriber of the index
 * @param {addressParam} sender Publisher or subscriber of the index
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 *
 * NOTE:
 * It means both revoking and clear the units of a subscription.
 */
    deleteSubscription({
        superToken,
        indexId,
        publisher,
        subscriber,
        sender,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: DeleteSubscriptionOptions): Promise<Transaction>;
    /**
     * @dev Get details of a subscription
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @param {addressParam} subscriber Subscriber of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber
    }: GetSubscriptionOptions): Promise<Subscription>;
/**
 * @dev Claim distributions to a subscriber of the index by anyone.
 * @param {tokenParam} superToken SuperToken for the index
 * @param {addressParam} publisher Publisher of the index
 * @param {int} indexId ID of the index
 * @param {addressParam} subscriber Subscriber of the index
 * @param {addressParam} sender Any account to claim the distribution for the subscriber
 * @param {Function} onTransaction function to be called when transaction hash has been generated
 * @param {GasOptions} gasOptions pass network gas parameters
 * @return {Promise<Transaction>} web3 transaction object
 *
 * NOTE:
 * If the subscriber has not approved the subscription, anyone can claim the distribution for him.
 */
    claim({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
        userData,
        onTransaction,
        gasOptions? : GasOptions,
    }: ClaimOptions): Promise<Transaction>;
    /**
     * @dev Get details of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    getIndex({
        superToken,
        publisher,
        indexId
    }: GetIndexOptions): Promise<IndexData>;
    /**
     * @dev List indices of a publisher
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listIndices({
        superToken,
        publisher
    }: ListIndicesOptions): Promise<number[]>;
    /**
     * @dev List subscribers of an index
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubcribers({
        superToken,
        publisher,
        indexId
    }: ListSubcribersOptions): Promise<SubscriberList>;
    /**
     * @dev List subscriptions of an account
     * @param {tokenParam} superToken SuperToken for the index
     * @param {addressParam} publisher Publisher of the index
     * @param {int} indexId ID of the index
     * @return {Promise<Subscription>} Subscription data
     */
    listSubscriptions({
        superToken,
        subscriber
    }: ListSubscriptionsOptions): Promise<SubscriptionInfoList>;
}
