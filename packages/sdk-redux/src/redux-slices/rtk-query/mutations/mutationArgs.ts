import {NothingString, SuperTokenMutationArg} from '../../argTypes';

/**
 * Approves a Subscription, so the Subscriber won't need to claim tokens when the Publisher distributes.
 * @param indexId The id of the index.
 * @param publisherAddress The publisher address whose subscription you want to approve.
 * @param userDataBytes Extra user data provided.
 */
export type ApproveIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    userDataBytes: string | NothingString;
};

/**
 * Claims any pending tokens allocated to the Subscription (unapproved).
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address who you are claiming for.
 * @param publisherAddress The publisher address of the index you are targeting.
 * @param userDataBytes Extra user data provided.
 */
export type ClaimFromIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
};

// TODO(KK): User data in arg
/**
 * Create a flow of the token of this class.
 * @param senderAddress The sender of the flow. Signer is used when left empty.
 * @param receiverAddress The receiver of the flow.
 * @param flowRateWei The specified flow rate.
 */
export type CreateFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
    flowRateWei: string;
};

/**
 * Creates an IDA Index.
 * @param indexId The id of the index.
 * @param userDataBytes Extra user data provided.
 */
export type CreateIndexArg = SuperTokenMutationArg & {
    indexId: string;
    userDataBytes: string;
};

// TODO(KK): Add user data
/**
 * Delete a flow of the token of this class.
 * @param senderAddress The sender of the flow.
 * @param receiverAddress The receiver of the flow.
 */
export type DeleteFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
};

/**
 * Deletes a Subscription by setting the `units` allocated to the Subscriber to 0.
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address whose subscription you want to delete.
 * @param publisherAddress The publisher address of the index you are targeting.
 * @param userDataBytes Extra user data provided.
 */
export type DeleteIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    subscriberAddress: string;
    userDataBytes: string | NothingString;
};

/**
 * Distributes `amount` of token to an index
 * @param indexId The id of the index.
 * @param amountWei The amount of tokens to be distributed.
 * @param userDataBytes Extra user data provided.
 */
export type DistributeToIndexArg = SuperTokenMutationArg & {
    indexId: string;
    amountWei: string;
    userDataBytes: string | NothingString;
};

/**
 * Downgrade `amount` SuperToken's.
 * @param amountWei The amount to be downgraded.
 */
export type DowngradeFromSuperTokenArg = SuperTokenMutationArg & {
    amountWei: string;
};

/**
 * Continuously poll for new events to know when to invalidate cache for re-fetching of the data.
 * @param chainId The chain to poll.
 * @param address The address (account or token) to filter events for.
 */
export type MonitorForEventsToInvalidateCacheArg = {
    chainId: number;
    address: string | NothingString;
};

/**
 * Revokes a Subscription, so the Subscriber will need to claim tokens when the Publisher distributes.
 * @param indexId The id of the index.
 * @param publisherAddress The index publisher address you want to revoke for the subscriber.
 * @param userDataBytes Extra user data provided.
 */
export type RevokeIndexSubscriptionArg = SuperTokenMutationArg & {
    indexId: string;
    publisherAddress: string;
    userDataBytes: string | NothingString;
};

/**
 * Transfer `receiver` `amount` tokens.
 * @param receiverAddress The receiver of the transfer.
 * @param amountWei The amount to be transferred.
 */
export type TransferSuperTokenArg = SuperTokenMutationArg & {
    receiverAddress: string;
    amountWei: string;
};

// TODO(KK): user bytes
/**
 * Update a flow of the token of this class.
 * @param senderAddress The sender of the flow.
 * @param receiverAddress The receiver of the flow.
 * @param flowRateWei The specified flow rate.
 */
export type UpdateFlowArg = SuperTokenMutationArg & {
    senderAddress?: string;
    receiverAddress: string;
    flowRateWei: string;
};

/**
 * Updates the `units` allocated to a Subscription.
 * @param indexId The id of the index.
 * @param subscriberAddress The subscriber address whose units you want to update.
 * @param unitsNumber The amount of units you want to update to.
 * @param userDataBytes Extra user data provided.
 */
export type UpdateIndexSubscriptionUnitsArg = SuperTokenMutationArg & {
    subscriberAddress: string;
    indexId: string;
    unitsNumber: string;
    userDataBytes: string | NothingString;
};

/**
 * Upgrade `amount` SuperToken's.
 * NOTE: Initiates request for allowance if necessary.
 * @param amountWei The amount to be upgraded.
 */
export type UpgradeToSuperTokenArg = SuperTokenMutationArg & {
    amountWei: string;
};
