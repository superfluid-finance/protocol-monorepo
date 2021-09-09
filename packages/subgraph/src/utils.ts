import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import {
    Account,
    Index,
    AccountTokenSnapshot,
    Stream,
    StreamRevision,
    Subscriber,
    TokenStats,
} from "../generated/schema";

/**************************************************************************
 * Constants
 *************************************************************************/

export const BIG_INT_ZERO = BigInt.fromI32(0);
export const BIG_INT_ONE = BigInt.fromI32(1);

/**************************************************************************
 * Event entities util functions
 *************************************************************************/

export function createEventID(event: ethereum.Event): string {
    return event.transaction.hash
        .toHex()
        .concat("-")
        .concat(event.logIndex.toString());
}

/**************************************************************************
 * HOL entities util functions
 *************************************************************************/

/**
 * Creates an Account entity if non exists or updates an existing one.
 * this should technically only be updated once as no property on account
 * changes currently.
 * @param id
 * @param lastModified
 * @returns created or modified account
 */
export function createOrUpdateAccount(
    id: string,
    lastModified: BigInt
): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
        account.createdAt = lastModified;
        account.updatedAt = lastModified;
    }
    account.save();
    return account;
}

function getStreamRevisionID(
    senderId: string,
    recipientId: string,
    tokenId: string
): string {
    return senderId.concat("-").concat(recipientId).concat("-").concat(tokenId);
}

export function getOrInitStreamRevision(
    senderId: string,
    recipientId: string,
    tokenId: string
): [StreamRevision, boolean] {
    let streamRevisionId = getStreamRevisionID(senderId, recipientId, tokenId);
    let streamRevision = StreamRevision.load(streamRevisionId);
    if (streamRevision == null) {
        streamRevision = new StreamRevision(streamRevisionId);
        streamRevision.revisionIndex = 0;
        return [streamRevision, false];
    }
    return [streamRevision, true];
}

function getStreamID(
    senderId: string,
    receiverId: string,
    tokenId: string,
    revisionIndex: number
): string {
    return getStreamRevisionID(senderId, receiverId, tokenId)
        .concat("-")
        .concat(revisionIndex.toString());
}

export function getOrInitStream(
    senderAddress: string,
    receiverAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Stream {
    // Create accounts if they do not exist
    createOrUpdateAccount(receiverAddress, timestamp);
    let [streamRevision, streamRevisionExists] = getOrInitStreamRevision(
        senderAddress,
        receiverAddress,
        tokenAddress
    );
    if (!streamRevisionExists) {
        streamRevision.save();
    }
    let id = getStreamID(
        senderAddress,
        receiverAddress,
        tokenAddress,
        streamRevision.revisionIndex
    );
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAt = timestamp;
        stream.token = tokenAddress;
        stream.sender = senderAddress;
        stream.receiver = receiverAddress;
        stream.currentFlowRate = BIG_INT_ZERO;
        stream.streamedUntilUpdatedAt = BIG_INT_ZERO;
    }
    return stream;
}

export function getSubscriberID(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): string {
    return (
        subscriberAddress.toHexString() +
        "-" +
        publisherAddress.toHexString() +
        "-" +
        tokenAddress.toHexString() +
        "-" +
        indexId.toString()
    );
}

export function getIndexID(
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): string {
    return (
        publisherAddress.toHex() +
        "-" +
        tokenAddress.toHex() +
        "-" +
        indexId.toString()
    );
}

export function getOrInitIndex(
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt,
    lastModified: BigInt
): Index {
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let index = Index.load(indexEntityId);
    if (index == null) {
        let publisherId = publisherAddress.toHex();
        index = new Index(indexEntityId);
        index.createdAt = lastModified;
        index.token = tokenAddress.toHex();
        index.publisher = publisherId;
        index.indexId = indexId;
        index.oldIndexValue = BIG_INT_ZERO;
        index.newIndexValue = BIG_INT_ZERO;
        index.totalUnitsPending = BIG_INT_ZERO;
        index.totalUnitsApproved = BIG_INT_ZERO;
        index.totalUnits = BIG_INT_ZERO;
        index.totalUnitsDistributed = BIG_INT_ZERO;

        createOrUpdateAccount(publisherId, lastModified);
    }
    index.updatedAt = lastModified;
    return index;
}

export function getOrInitSubscriber(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt,
    lastModified: BigInt
): [Subscriber, boolean] {
    let subscriberEntityId = getSubscriberID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscriber = Subscriber.load(subscriberEntityId);
    if (subscriber == null) {
        let subscriberId = subscriberAddress.toHex();
        subscriber = new Subscriber(subscriberEntityId);
        subscriber.createdAt = lastModified;
        subscriber.updatedAt = lastModified;
        subscriber.token = tokenAddress.toHex();
        subscriber.subscriber = subscriberId;
        subscriber.publisher = publisherAddress.toHex();
        subscriber.indexId = indexId;
        subscriber.approved = false;
        subscriber.units = BIG_INT_ZERO;
        subscriber.totalUnitsReceivedUntilUpdatedAt = BIG_INT_ZERO;
        subscriber.totalUnitsPendingApproval = BIG_INT_ZERO;
        subscriber.index = getIndexID(publisherAddress, tokenAddress, indexId);

        createOrUpdateAccount(subscriberId, lastModified);
        return [subscriber, false];
    }
    subscriber.updatedAt = lastModified;
    return [subscriber, true];
}

/**************************************************************************
 * Aggregate Entities Helper Functions
 *************************************************************************/

function getAccountTokenSnapshotID(accountId: string, tokenId: string): string {
    return accountId.concat("-").concat(tokenId);
}

export function getOrInitAccountTokenSnapshot(
    accountId: string,
    tokenId: string
): AccountTokenSnapshot {
    let accountTokenSnapshotId = getAccountTokenSnapshotID(accountId, tokenId);
    let accountTokenSnapshot = AccountTokenSnapshot.load(
        accountTokenSnapshotId
    );
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(accountTokenSnapshotId);
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
        accountTokenSnapshot.balance = BIG_INT_ZERO;
        accountTokenSnapshot.totalNumberOfStreams = BIG_INT_ZERO;
        accountTokenSnapshot.totalUnitsPending = BIG_INT_ZERO;
        accountTokenSnapshot.totalUnitsReceived = BIG_INT_ZERO;
        accountTokenSnapshot.totalSubscriptions = BIG_INT_ZERO;
        accountTokenSnapshot.totalApprovedSubscriptions = BIG_INT_ZERO;
    }
    return accountTokenSnapshot;
}

/**
 * Updates the balance property on the ATS entity.
 * Note: ATS = AccountTokenSnapshot
 * @param accountId
 * @param tokenId
 */
export function updateATSBalance(accountId: string, tokenId: string): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId
    );
    log.info("Token updateBalance: {}", [tokenId]);
    let superTokenContract = SuperToken.bind(Address.fromString(tokenId));
    let newBalance = superTokenContract.balanceOf(
        Address.fromString(accountId)
    );
    accountTokenSnapshot.balance = newBalance;
    accountTokenSnapshot.save();
}

/**
 * Updates the net flow rates of the sender and receiver on their respective
 * ATS entities.
 * Note: ATS = AccountTokenSnapshot
 * @param senderId
 * @param receiverId
 * @param tokenId
 * @param flowRateDelta
 */
export function updateATSFlowRates(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt
): void {
    let senderAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
        senderId,
        tokenId
    );
    let receiverAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
        receiverId,
        tokenId
    );

    senderAccountTokenSnapshot.totalNetFlowRate =
        senderAccountTokenSnapshot.totalNetFlowRate.plus(flowRateDelta.neg());
    receiverAccountTokenSnapshot.totalNetFlowRate =
        senderAccountTokenSnapshot.totalNetFlowRate.plus(flowRateDelta);

    senderAccountTokenSnapshot.save();
    receiverAccountTokenSnapshot.save();
}

export function updateATSIDAUnitsData(
    accountId: string,
    tokenId: string,
    totalUnitsReceivedDelta: BigInt,
    totalUnitsPendingDelta: BigInt
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId
    );
    accountTokenSnapshot.totalUnitsReceived =
        accountTokenSnapshot.totalUnitsReceived.plus(totalUnitsReceivedDelta);
    accountTokenSnapshot.totalUnitsPending =
        accountTokenSnapshot.totalUnitsPending.plus(totalUnitsPendingDelta);
    accountTokenSnapshot.save();
}

export function getOrInitTokenStats(tokenId: string): TokenStats {
    let tokenStats = TokenStats.load(tokenId);
    if (tokenStats == null) {
        tokenStats = new TokenStats(tokenId);
        tokenStats.token = tokenId;
        tokenStats.totalNumberOfStreams = BIG_INT_ZERO;
        tokenStats.totalNumberOfIndexes = BIG_INT_ZERO;
        tokenStats.totalOutflowRate = BIG_INT_ZERO;
        tokenStats.totalUnitsApproved = BIG_INT_ZERO;
        tokenStats.totalUnitsPending = BIG_INT_ZERO;
        tokenStats.totalUnitsDistributed = BIG_INT_ZERO;
        tokenStats.totalSubscribers = BIG_INT_ZERO;
        tokenStats.totalApprovedSubscribers = BIG_INT_ZERO;
    }
    return tokenStats;
}

/**
 * Updates the totalUnitsApproved and totalUnitsPending
 * properties on the TokenStats aggregate entity.
 * @param tokenId
 * @param totalUnitsApprovedDelta
 * @param totalUnitsPendingDelta
 */
export function updateTokenStatsIDAUnitsData(
    tokenId: string,
    totalUnitsApprovedDelta: BigInt,
    totalUnitsPendingDelta: BigInt
): void {
    let tokenStats = getOrInitTokenStats(tokenId);
    tokenStats.totalUnitsApproved = tokenStats.totalUnitsApproved.plus(
        totalUnitsApprovedDelta
    );
    tokenStats.totalUnitsPending = tokenStats.totalUnitsPending.plus(
        totalUnitsPendingDelta
    );
    tokenStats.save();
}

export function updateAggregateIDASubscriptionsData(
    accountId: string,
    tokenId: string,
    subscriptionExists: boolean,
    isDeletingSubscription: boolean,
    isApproving: boolean
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId
    );
    let tokenStats = getOrInitTokenStats(tokenId);
    let totalSubscriptionsDelta = isDeletingSubscription
        ? accountTokenSnapshot.totalSubscriptions.minus(BIG_INT_ONE)
        : subscriptionExists
        ? accountTokenSnapshot.totalSubscriptions
        : accountTokenSnapshot.totalSubscriptions.plus(BIG_INT_ONE);
    let totalApprovedSubscriptionsDelta = isApproving
        ? accountTokenSnapshot.totalApprovedSubscriptions.plus(BIG_INT_ONE)
        : accountTokenSnapshot.totalApprovedSubscriptions.minus(BIG_INT_ONE);

    // update ATS Subscriber data
    accountTokenSnapshot.totalSubscriptions =
        accountTokenSnapshot.totalSubscriptions.plus(totalSubscriptionsDelta);
    accountTokenSnapshot.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions.plus(
            totalApprovedSubscriptionsDelta
        );

    // update TokenStats Subscriber data
    tokenStats.totalSubscribers = tokenStats.totalSubscribers.plus(
        totalSubscriptionsDelta
    );
    tokenStats.totalApprovedSubscribers =
        tokenStats.totalApprovedSubscribers.plus(
            totalApprovedSubscriptionsDelta
        );
    accountTokenSnapshot.save();
    tokenStats.save();
}

export function updateAggregateEntityStreamData(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean
): void {
    let tokenStats = getOrInitTokenStats(tokenId);
    let totalNumberOfStreamsDelta = isCreate
        ? BIG_INT_ONE
        : isDelete
        ? BIG_INT_ONE.neg()
        : BIG_INT_ZERO;
    tokenStats.totalOutflowRate =
        tokenStats.totalOutflowRate.plus(flowRateDelta);
    tokenStats.totalNumberOfStreams = tokenStats.totalNumberOfStreams.plus(
        totalNumberOfStreamsDelta
    );
    tokenStats.save();

    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId);
    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId);
    receiverATS.totalNumberOfStreams = receiverATS.totalNumberOfStreams.plus(
        totalNumberOfStreamsDelta
    );
    senderATS.totalNumberOfStreams = senderATS.totalNumberOfStreams.plus(
        totalNumberOfStreamsDelta
    );
}
