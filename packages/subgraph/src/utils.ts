import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import {
    Account,
    Index,
    AccountTokenSnapshot,
    Stream,
    StreamRevision,
    Subscriber,
    TokenStatistic,
    Token,
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
 * Gets the Account entity with id or creates one with it. This should only
 * be called one time as account holds no dynamic state.
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

/**
 * Creates a HOL Token (SuperToken) entity if non exists, this function should
 * never be called more than once for the Token entity (you only create a
 * SuperToken once). We also create tkoen stats in here if it doesn't exist yet.
 * @param tokenId
 * @param lastModified
 * @returns created token
 */
export function getOrInitToken(tokenId: string, lastModified: BigInt): Token {
    let token = Token.load(tokenId);
    if (token == null) {
        let tokenContract = SuperToken.bind(Address.fromString(tokenId));
        let underlyingAddress = tokenContract.getUnderlyingToken();
        let name = tokenContract.name();
        let symbol = tokenContract.symbol();
        token = new Token(tokenId);
        token.createdAt = lastModified;
        token.updatedAt = lastModified;
        token.name = name;
        token.symbol = symbol;
        token.underlyingAddress = underlyingAddress;
        token.save();

        // Note: we initalize and create tokenStatistic whenever we create a
        // token as well.
        let tokenStatistic = getOrInitTokenStatistic(tokenId);
        tokenStatistic.save();
    }
    return token as Token;
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

export function getOrInitStream(
    senderId: string,
    receiverId: string,
    tokenId: string,
    lastModified: BigInt
): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(senderId, lastModified);
    getOrInitAccount(receiverId, lastModified);

    // Create a streamRevision entity for this stream if one doesn't exist.
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
        stream.createdAt = lastModified;
        stream.updatedAt = lastModified;
        stream.token = tokenId;
        stream.sender = senderId;
        stream.receiver = receiverId;
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        if (!tokenExists(tokenId)) {
            getOrInitToken(tokenId, lastModified);
        }
    }
    return stream as Stream;
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
        let tokenId = tokenAddress.toHex();
        index = new Index(indexEntityId);
        index.createdAt = lastModified;
        index.indexId = indexId;
        index.userData = new Bytes(0);
        index.oldIndexValue = BIG_INT_ZERO;
        index.newIndexValue = BIG_INT_ZERO;
        index.totalSubscribers = 0;
        index.totalUnitsPending = BIG_INT_ZERO;
        index.totalUnitsApproved = BIG_INT_ZERO;
        index.totalUnits = BIG_INT_ZERO;
        index.totalUnitsDistributed = BIG_INT_ZERO;
        index.token = tokenId;
        index.publisher = publisherId;

        getOrInitAccount(publisherId, lastModified);

        // NOTE: we must check if token exists and create here
        // if not. for SETH tokens (e.g. ETH, MATIC, xDAI)
        if (!tokenExists(tokenId)) {
            getOrInitToken(tokenId, lastModified);
        }
    }
    index.updatedAt = lastModified;
    return index as Index;
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
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);

    if (subscriber == null) {
        let index = getOrInitIndex(
            publisherAddress,
            tokenAddress,
            indexId,
            lastModified
        );
        index.totalSubscribers = index.totalSubscribers + 1;
        index.save();

        let subscriberId = subscriberAddress.toHex();
        subscriber = new Subscriber(subscriberEntityId);
        subscriber.createdAt = lastModified;
        subscriber.token = tokenAddress.toHex();
        subscriber.subscriber = subscriberId;
        subscriber.publisher = publisherAddress.toHex();
        subscriber.indexId = indexId;
        subscriber.userData = new Bytes(0);
        subscriber.approved = false;
        subscriber.units = BIG_INT_ZERO;
        subscriber.totalUnitsReceivedUntilUpdatedAt = BIG_INT_ZERO;
        subscriber.lastIndexValue = index.newIndexValue;
        subscriber.index = indexEntityId;

        getOrInitAccount(subscriberId, lastModified);
    }
    subscriber.updatedAt = lastModified;
    return subscriber as Subscriber;
}

// Get HOL ID functions
function getStreamRevisionID(
    senderId: string,
    recipientId: string,
    tokenId: string
): string {
    return senderId.concat("-").concat(recipientId).concat("-").concat(tokenId);
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

export function getSubscriberID(
    subscriberAddress: Bytes,
    publisherAddress: Bytes,
    tokenAddress: Bytes,
    indexId: BigInt
): string {
    return (
        subscriberAddress.toHex() +
        "-" +
        publisherAddress.toHex() +
        "-" +
        tokenAddress.toHex() +
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

// Get HOL Exists Functions
export function tokenExists(id: string): boolean {
    return Token.load(id) != null;
}

export function streamRevisionExists(id: string): boolean {
    return StreamRevision.load(id) != null;
}

export function subscriptionExists(id: string): boolean {
    return Subscriber.load(id) != null;
}

/**************************************************************************
 * Aggregate Entities Helper Functions
 *************************************************************************/

export function getOrInitAccountTokenSnapshot(
    accountId: string,
    tokenId: string
): AccountTokenSnapshot {
    let atsId = getAccountTokenSnapshotID(accountId, tokenId);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.totalNumberOfStreams = 0;
        accountTokenSnapshot.totalSubscriptions = 0;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.balance = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
    }
    return accountTokenSnapshot as AccountTokenSnapshot;
}

export function getOrInitTokenStatistic(tokenId: string): TokenStatistic {
    let tokenStatistic = TokenStatistic.load(tokenId);
    if (tokenStatistic == null) {
        tokenStatistic = new TokenStatistic(tokenId);
        tokenStatistic.totalNumberOfStreams = 0;
        tokenStatistic.totalNumberOfIndexes = 0;
        tokenStatistic.totalSubscribers = 0;
        tokenStatistic.totalApprovedSubscribers = 0;
        tokenStatistic.totalOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalUnitsApproved = BIG_INT_ZERO;
        tokenStatistic.totalUnitsPending = BIG_INT_ZERO;
        tokenStatistic.totalUnitsDistributed = BIG_INT_ZERO;
        tokenStatistic.token = tokenId;
    }
    return tokenStatistic as TokenStatistic;
}

/**
 * Updates the totalUnitsApproved and totalUnitsPending
 * properties on the TokenStatistic aggregate entity.
 * @param tokenId
 * @param totalUnitsApprovedDelta
 * @param totalUnitsPendingDelta
 */
export function updateTokenStatisticIDAUnitsData(
    tokenId: string,
    totalUnitsApprovedDelta: BigInt,
    totalUnitsPendingDelta: BigInt
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenId);
    tokenStatistic.totalUnitsApproved = tokenStatistic.totalUnitsApproved.plus(
        totalUnitsApprovedDelta
    );
    tokenStatistic.totalUnitsPending = tokenStatistic.totalUnitsPending.plus(
        totalUnitsPendingDelta
    );
    tokenStatistic.save();
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
    let tokenStatistic = getOrInitTokenStatistic(tokenId);
    let totalSubscriptionsDelta = isDeletingSubscription
        ? -1
        : subscriptionExists
        ? 0
        : 1;
    let totalApprovedSubscriptionsDelta = isApproving
        ? 1
        : subscriptionExists
        ? -1
        : 0;

    // update ATS Subscriber data
    accountTokenSnapshot.totalSubscriptions =
        accountTokenSnapshot.totalSubscriptions + totalSubscriptionsDelta;
    accountTokenSnapshot.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;

    // update tokenStatistic Subscriber data
    tokenStatistic.totalSubscribers =
        tokenStatistic.totalSubscribers + totalSubscriptionsDelta;
    tokenStatistic.totalApprovedSubscribers =
        tokenStatistic.totalApprovedSubscribers +
        totalApprovedSubscriptionsDelta;

    accountTokenSnapshot.save();
    tokenStatistic.save();
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

export function updateAggregateEntitiesStreamData(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenId);
    let totalNumberOfStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    tokenStatistic.totalOutflowRate =
        tokenStatistic.totalOutflowRate.plus(flowRateDelta);
    tokenStatistic.totalNumberOfStreams =
        tokenStatistic.totalNumberOfStreams + totalNumberOfStreamsDelta;
    tokenStatistic.save();

    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId);
    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId);
    receiverATS.totalNumberOfStreams =
        receiverATS.totalNumberOfStreams + totalNumberOfStreamsDelta;
    senderATS.totalNumberOfStreams =
        senderATS.totalNumberOfStreams + totalNumberOfStreamsDelta;
    receiverATS.save();
    senderATS.save();
}

// Get Aggregate ID functions
function getAccountTokenSnapshotID(accountId: string, tokenId: string): string {
    return accountId.concat("-").concat(tokenId);
}
