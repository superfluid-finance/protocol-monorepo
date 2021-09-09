import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import {
    Account,
    Stream,
    Subscriber,
    Index,
    AccountTokenSnapshot,
    TokenStats,
} from "../generated/schema";

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
        account._autoIncrement = 0;
    }
    account.save();
    return account;
}

// TODO: add _autoIncrement for ordering?
function getStreamID(
    owner: string,
    recipient: string,
    token: string,
    autoIncrement: number
): string {
    return owner
        .concat("-")
        .concat(recipient)
        .concat("-")
        .concat(token)
        .concat("-")
        .concat(autoIncrement.toString());
}

export function getStream(
    senderAddress: string,
    receiverAddress: string,
    tokenAddress: string,
    timestamp: BigInt
): Stream {
    // Create accounts if they do not exist
    let senderAccount = createOrUpdateAccount(senderAddress, timestamp);
    createOrUpdateAccount(receiverAddress, timestamp);
    let id = getStreamID(
        senderAddress,
        receiverAddress,
        tokenAddress,
        senderAccount._autoIncrement
    );
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAt = timestamp;
        stream.token = tokenAddress;
        stream.sender = senderAddress;
        stream.receiver = receiverAddress;
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilLastUpdate = BigInt.fromI32(0);
    }
    return stream;
}
// TODO: add _autoIncrement for ordering?
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

// TODO: add _autoIncrement for ordering?
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
        index.oldIndexValue = BigInt.fromI32(0);
        index.newIndexValue = BigInt.fromI32(0);
        index.totalUnitsPending = BigInt.fromI32(0);
        index.totalUnitsApproved = BigInt.fromI32(0);
        index.totalUnits = BigInt.fromI32(0);
        index.totalUnitsDistributed = BigInt.fromI32(0);

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
        subscriber.units = BigInt.fromI32(0);
        subscriber.totalReceivedUntilLastUpdate = BigInt.fromI32(0);
        subscriber.totalPendingApproval = BigInt.fromI32(0);
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
    // TODO: constants for BigInt 0 and 1
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(accountTokenSnapshotId);
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
        accountTokenSnapshot.balance = BigInt.fromI32(0);
        accountTokenSnapshot.totalNumberOfStreams = BigInt.fromI32(0);
        accountTokenSnapshot.totalUnitsPending = BigInt.fromI32(0);
        accountTokenSnapshot.totalUnitsReceived = BigInt.fromI32(0);
        accountTokenSnapshot.totalSubscriptions = BigInt.fromI32(0);
        accountTokenSnapshot.totalApprovedSubscriptions = BigInt.fromI32(0);
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
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(accountId, tokenId);
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
    let senderAccountTokenSnapshot = getOrInitAccountTokenSnapshot(senderId, tokenId);
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
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(accountId, tokenId);
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
        tokenStats.totalNumberOfStreams = BigInt.fromI32(0);
        tokenStats.totalNumberOfIndexes = BigInt.fromI32(0);
        tokenStats.totalOutflowRate = BigInt.fromI32(0);
        tokenStats.totalUnitsApproved = BigInt.fromI32(0);
        tokenStats.totalUnitsPending = BigInt.fromI32(0);
        tokenStats.totalUnitsDistributed = BigInt.fromI32(0);
        tokenStats.totalSubscribers = BigInt.fromI32(0);
        tokenStats.totalApprovedSubscribers = BigInt.fromI32(0);
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
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(accountId, tokenId);
    let tokenStats = getOrInitTokenStats(tokenId);
    let bigIntOne = BigInt.fromI32(1);
    let totalSubscriptionsDelta = isDeletingSubscription
        ? accountTokenSnapshot.totalSubscriptions.minus(bigIntOne)
        : subscriptionExists
        ? accountTokenSnapshot.totalSubscriptions
        : accountTokenSnapshot.totalSubscriptions.plus(bigIntOne);
    let totalApprovedSubscriptionsDelta = isApproving
        ? accountTokenSnapshot.totalApprovedSubscriptions.plus(bigIntOne)
        : accountTokenSnapshot.totalApprovedSubscriptions.minus(bigIntOne);

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
        ? BigInt.fromI32(1)
        : isDelete
        ? BigInt.fromI32(-1)
        : BigInt.fromI32(0);
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
