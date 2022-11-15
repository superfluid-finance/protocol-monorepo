import {BaseSuperTokenMutation, NothingString} from '../../argTypes';

/**
 * Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
 */
export interface ApproveIndexSubscription extends BaseSuperTokenMutation {
    indexId: string;
    publisherAddress: string;
    userDataBytes: string | NothingString;
}

/**
 * Claims any pending tokens allocated to the Subscription (unapproved).
 */
export interface ClaimFromIndexSubscription extends BaseSuperTokenMutation {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
}

// TODO(KK): User data in arg
/**
 * Create a flow of the token of this class.
 */
export interface CreateFlow extends BaseSuperTokenMutation {
    /** The sender of the flow. Signer is used when left empty. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
    /** The specified flow rate. */
    flowRateWei: string;
}

/**
 * Creates an IDA Index.
 */
export interface CreateIndex extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** Extra user data provided. */
    userDataBytes: string;
}

// TODO(KK): Add user data
/**
 * Delete a flow of the token of this class.
 */
export interface DeleteFlow extends BaseSuperTokenMutation {
    /** The sender of the flow. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
}

/**
 * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
 */
export interface DeleteIndexSubscription extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The subscriber address whose subscription you want to delete. */
    publisherAddress: string;
    /** The publisher address of the index you are targeting. */
    subscriberAddress: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Distributes `amount` of token to an index
 */
export interface DistributeToIndex extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The amount of tokens to be distributed. */
    amountWei: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Downgrade `amount` SuperToken's.
 */
export interface DowngradeFromSuperToken extends BaseSuperTokenMutation {
    /** The amount to be downgraded. */
    amountWei: string;
}

/**
 * Continuously poll for new events to know when to invalidate cache for re-fetching of the data.
 */
export interface MonitorForEventsToInvalidateCache {
    /** The chain to poll. */
    chainId: number;
    /** The address (account or token) to filter events for. */
    address: string | NothingString;
}

/**
 * Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
 */
export interface RevokeIndexSubscription extends BaseSuperTokenMutation {
    /** The id of the index. */
    indexId: string;
    /** The index publisher address you want to revoke for the subscriber. */
    publisherAddress: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Transfer `receiver` `amount` tokens.
 */
export interface TransferSuperToken extends BaseSuperTokenMutation {
    /** The receiver of the transfer. */
    receiverAddress: string;
    /** The amount to be transferred. */
    amountWei: string;
}

// TODO(KK): user bytes
/**
 * Update a flow of the token of this class.
 */
export interface UpdateFlow extends BaseSuperTokenMutation {
    /** The sender of the flow. If not specified then signer address is used. */
    senderAddress?: string;
    /** The receiver of the flow. */
    receiverAddress: string;
    /** The specified flow rate. */
    flowRateWei: string;
}

/**
 * Updates the `units` allocated to a Subscription.
 */
export interface UpdateIndexSubscriptionUnits extends BaseSuperTokenMutation {
    /** The subscriber address whose units you want to update. */
    subscriberAddress: string;
    /** The id of the index. */
    indexId: string;
    /** The amount of units you want to update to. */
    unitsNumber: string;
    /** Extra user data provided. */
    userDataBytes: string | NothingString;
}

/**
 * Upgrade `amount` SuperToken's.
 * NOTE: Initiates request for allowance if necessary.
 */
export interface UpgradeToSuperToken extends BaseSuperTokenMutation {
    /** The amount to be upgraded. */
    amountWei: string;
}
