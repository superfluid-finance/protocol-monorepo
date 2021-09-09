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

export let BIG_INT_ZERO = BigInt.fromI32(0);
export let BIG_INT_ONE = BigInt.fromI32(1);

/**************************************************************************
 * Event entities util functions
 *************************************************************************/

export function createEventID(event: ethereum.Event): string {
    return event.transaction.hash
        .toHexString()
        .concat("-")
        .concat(event.logIndex.toString());
}

/**************************************************************************
 * HOL entities util functions
 *************************************************************************/

/**
 * Creates an Account entity if non exists or updates an existing one.
 * this should technically only be "updated" on creation as account
 * holds no dynamic state.
 * @param id
 * @param lastModified
 * @returns created or modified account
 */
export function getOrInitAccount(id: string, lastModified: BigInt): Account {
    let account = Account.load(id);
    if (account == null) {
        account = new Account(id);
        account.createdAt = lastModified;
        account.updatedAt = lastModified;
        account.save();
    }
    return account as Account;
}

function getStreamRevisionID(
    senderId: string,
    recipientId: string,
    tokenId: string
): string {
    return senderId.concat("-").concat(recipientId).concat("-").concat(tokenId);
}

export function streamRevisionExists(id: string): boolean {
    return StreamRevision.load(id) != null;
}

/**
 * Gets or initializes the Stream Revision helper entity.
 * @param senderId
 * @param recipientId
 * @param tokenId
 * @returns [StreamRevision, boolean: whether the streamRevision existed]
 */
export function getOrInitStreamRevision(
    senderId: string,
    recipientId: string,
    tokenId: string
): StreamRevision {
    let streamRevisionId = getStreamRevisionID(senderId, recipientId, tokenId);
    let streamRevision = StreamRevision.load(streamRevisionId);
    if (streamRevision == null) {
        streamRevision = new StreamRevision(streamRevisionId);
        streamRevision.revisionIndex = 0;
    }
    return streamRevision as StreamRevision;
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
    senderId: string,
    receiverId: string,
    tokenId: string,
    timestamp: BigInt
): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(senderId, timestamp);
    getOrInitAccount(receiverId, timestamp);
    let streamRevision = getOrInitStreamRevision(senderId, receiverId, tokenId);
    if (
        !streamRevisionExists(
            getStreamRevisionID(senderId, receiverId, tokenId)
        )
    ) {
        streamRevision.save();
    }
    let id = getStreamID(
        senderId,
        receiverId,
        tokenId,
        streamRevision.revisionIndex
    );
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAt = timestamp;
        stream.updatedAt = timestamp;
        stream.token = tokenId;
        stream.sender = senderId;
        stream.receiver = receiverId;
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);
    }
    return stream as Stream;
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
        index.totalSubscribers = 0;
        index.oldIndexValue = BIG_INT_ZERO;
        index.newIndexValue = BIG_INT_ZERO;
        index.totalUnitsPending = BIG_INT_ZERO;
        index.totalUnitsApproved = BIG_INT_ZERO;
        index.totalUnits = BIG_INT_ZERO;
        index.totalUnitsDistributed = BIG_INT_ZERO;

        getOrInitAccount(publisherId, lastModified);
    }
    index.updatedAt = lastModified;
    return index as Index;
}

export function subscriptionExists(id: string): boolean {
    return Subscriber.load(id) != null;
}

export function getOrInitSubscriber(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt,
    lastModified: BigInt
): Subscriber {
    let subscriberEntityId = getSubscriberID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscriber = Subscriber.load(subscriberEntityId);
    let index = Index.load(getIndexID(publisherAddress, tokenAddress, indexId));
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
        subscriber.index = getIndexID(publisherAddress, tokenAddress, indexId);
        index.totalSubscribers = index.totalSubscribers + 1;

        getOrInitAccount(subscriberId, lastModified);
    }
    subscriber.updatedAt = lastModified;
    return subscriber as Subscriber;
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
    let atsId = getAccountTokenSnapshotID(accountId, tokenId);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
        accountTokenSnapshot.balance = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalNumberOfStreams = 0;
        accountTokenSnapshot.totalSubscriptions = 0;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
    }
    return accountTokenSnapshot as AccountTokenSnapshot;
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
    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId);
    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId);

    senderATS.totalNetFlowRate =
        senderATS.totalNetFlowRate.minus(flowRateDelta);
    receiverATS.totalNetFlowRate =
        senderATS.totalNetFlowRate.plus(flowRateDelta);

    senderATS.save();
    receiverATS.save();
}

export function getOrInitTokenStats(tokenId: string): TokenStats {
    let tokenStats = TokenStats.load(tokenId);
    if (tokenStats == null) {
        tokenStats = new TokenStats(tokenId);
        tokenStats.token = tokenId;
        tokenStats.totalNumberOfStreams = 0;
        tokenStats.totalNumberOfIndexes = 0;
        tokenStats.totalSubscribers = 0;
        tokenStats.totalApprovedSubscribers = 0;
        tokenStats.totalOutflowRate = BIG_INT_ZERO;
        tokenStats.totalUnitsApproved = BIG_INT_ZERO;
        tokenStats.totalUnitsPending = BIG_INT_ZERO;
        tokenStats.totalUnitsDistributed = BIG_INT_ZERO;
    }
    return tokenStats as TokenStats;
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
        ? accountTokenSnapshot.totalSubscriptions - 1
        : subscriptionExists
        ? accountTokenSnapshot.totalSubscriptions
        : accountTokenSnapshot.totalSubscriptions + 1;
    let totalApprovedSubscriptionsDelta = isApproving
        ? accountTokenSnapshot.totalApprovedSubscriptions + 1
        : accountTokenSnapshot.totalApprovedSubscriptions - 1;

    // update ATS Subscriber data
    accountTokenSnapshot.totalSubscriptions =
        accountTokenSnapshot.totalSubscriptions + totalSubscriptionsDelta;
    accountTokenSnapshot.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;

    // update TokenStats Subscriber data
    tokenStats.totalSubscribers =
        tokenStats.totalSubscribers + totalSubscriptionsDelta;
    tokenStats.totalApprovedSubscribers =
        tokenStats.totalApprovedSubscribers + totalApprovedSubscriptionsDelta;
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
    let totalNumberOfStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    tokenStats.totalOutflowRate =
        tokenStats.totalOutflowRate.plus(flowRateDelta);
    tokenStats.totalNumberOfStreams =
        tokenStats.totalNumberOfStreams + totalNumberOfStreamsDelta;
    tokenStats.save();

    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId);
    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId);
    receiverATS.totalNumberOfStreams =
        receiverATS.totalNumberOfStreams + totalNumberOfStreamsDelta;
    senderATS.totalNumberOfStreams =
        senderATS.totalNumberOfStreams + totalNumberOfStreamsDelta;
    receiverATS.save();
    senderATS.save();
}
