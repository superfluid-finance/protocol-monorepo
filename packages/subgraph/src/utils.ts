import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { ISuperfluid as Superfluid } from "../generated/Host/ISuperfluid";
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
export function getOrInitAccount(
    event: ethereum.Event,
    accountAddress: Address,
    lastModified: BigInt
): Account {
    let account = Account.load(accountAddress.toHex());
    if (account == null) {
        let hostContract = Superfluid.bind(event.address);
        hostContract = Superfluid.bind(hostContract._address);
        let appManifest = hostContract.getAppManifest(accountAddress);
        account = new Account(accountAddress.toHex());
        account.createdAt = lastModified;
        account.updatedAt = lastModified;
        account.isSuperApp = appManifest.value0;
        account.save();
    }
    return account as Account;
}

/**
 * Creates a HOL Token (SuperToken) entity if non exists, this function should
 * never be called more than once for the Token entity (you only create a
 * SuperToken once). We also create token stats in here if it doesn't exist yet.
 * @param tokenAddress
 * @param lastModified
 * @returns created token
 */
export function getOrInitToken(
    event: ethereum.Event,
    tokenAddress: Address,
    lastModified: BigInt
): Token {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    if (token == null) {
        let tokenContract = SuperToken.bind(tokenAddress);
        let hostContract = Superfluid.bind(event.address);
        let tokenHostAddress = tokenContract.getHost();

        // if the host contract of the token is our host contract,
        // we will create token and tokenStatistic entities.
        if (tokenHostAddress.toHex() == hostContract._address.toHex()) {
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
            let tokenStatistic = getOrInitTokenStatistic(tokenId, lastModified);
            tokenStatistic.save();
        }
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
    let streamRevisionId = getStreamRevisionPrefix(
        senderId,
        recipientId,
        tokenId
    );
    let streamRevision = StreamRevision.load(streamRevisionId);
    if (streamRevision == null) {
        streamRevision = new StreamRevision(streamRevisionId);
        streamRevision.revisionIndex = 0;
    }
    return streamRevision as StreamRevision;
}

/**
 * Gets or initializes a Stream, always sets the updatedAt.
 * @param event
 * @param senderAddress
 * @param receiverAddress
 * @param tokenAddress
 * @param lastModified
 * @returns
 */
export function getOrInitStream(
    event: ethereum.Event,
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address,
    lastModified: BigInt
): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(event, senderAddress, lastModified);
    getOrInitAccount(event, receiverAddress, lastModified);

    // Create a streamRevision entity for this stream if one doesn't exist.
    let streamRevision = getOrInitStreamRevision(
        senderAddress.toHex(),
        receiverAddress.toHex(),
        tokenAddress.toHex()
    );
    if (
        !streamRevisionExists(
            getStreamRevisionPrefix(
                senderAddress.toHex(),
                receiverAddress.toHex(),
                tokenAddress.toHex()
            )
        )
    ) {
        streamRevision.save();
    }
    let id = getStreamID(
        senderAddress.toHex(),
        receiverAddress.toHex(),
        tokenAddress.toHex(),
        streamRevision.revisionIndex
    );
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAt = lastModified;
        stream.token = tokenAddress.toHex();
        stream.sender = senderAddress.toHex();
        stream.receiver = receiverAddress.toHex();
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        if (!tokenExists(tokenAddress.toHex())) {
            getOrInitToken(event, tokenAddress, lastModified);
        }
    }
    stream.updatedAt = lastModified;
    return stream as Stream;
}

/**
 * Gets or initializes an Index, always sets the updatedAt.
 * @param event
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param lastModified
 * @returns
 */
export function getOrInitIndex(
    event: ethereum.Event,
    publisherAddress: Address,
    tokenAddress: Address,
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
        index.totalAmountDistributed = BIG_INT_ZERO;
        index.token = tokenId;
        index.publisher = publisherId;

        getOrInitAccount(event, publisherAddress, lastModified);

        // NOTE: we must check if token exists and create here
        // if not. for SETH tokens (e.g. ETH, MATIC, xDAI)
        if (!tokenExists(tokenId)) {
            getOrInitToken(event, tokenAddress, lastModified);
        }
    }
    index.updatedAt = lastModified;
    return index as Index;
}

/**
 * Gets or initializes a Subscriber, always sets the updatedAt.
 * @param event
 * @param subscriberAddress
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param lastModified
 * @returns
 */
export function getOrInitSubscriber(
    event: ethereum.Event,
    subscriberAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
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
            event,
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
        subscriber.totalAmountReceivedUntilUpdatedAt = BIG_INT_ZERO;
        subscriber.lastIndexValue = index.newIndexValue;
        subscriber.index = indexEntityId;

        getOrInitAccount(event, subscriberAddress, lastModified);
    }
    subscriber.updatedAt = lastModified;
    return subscriber as Subscriber;
}

/**
 * Updates the Account entities updatedAt property.
 * @param event
 * @param accountAddress
 */
export function updateAccountUpdatedAt(
    event: ethereum.Event,
    accountAddress: Address
): void {
    let lastModified = event.block.timestamp;
    let account = getOrInitAccount(event, accountAddress, lastModified);
    account.updatedAt = lastModified;
    account.save();
}

// Get HOL ID functions
function getStreamRevisionPrefix(
    senderId: string,
    receiverId: string,
    tokenId: string
): string {
    return senderId.concat("-").concat(receiverId).concat("-").concat(tokenId);
}

function getStreamID(
    senderId: string,
    receiverId: string,
    tokenId: string,
    revisionIndex: number
): string {
    return getStreamRevisionPrefix(senderId, receiverId, tokenId)
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
    tokenId: string,
    lastModified: BigInt
): AccountTokenSnapshot {
    let atsId = getAccountTokenSnapshotID(accountId, tokenId);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.updatedAt = lastModified;
        accountTokenSnapshot.totalNumberOfActiveStreams = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalSubscriptions = 0;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.balance = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalInflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountTransferred = BIG_INT_ZERO;
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
    }
    return accountTokenSnapshot as AccountTokenSnapshot;
}

export function getOrInitTokenStatistic(
    tokenId: string,
    lastModified: BigInt
): TokenStatistic {
    let tokenStatistic = TokenStatistic.load(tokenId);
    if (tokenStatistic == null) {
        tokenStatistic = new TokenStatistic(tokenId);
        tokenStatistic.updatedAt = lastModified;
        tokenStatistic.totalNumberOfActiveStreams = 0;
        tokenStatistic.totalNumberOfClosedStreams = 0;
        tokenStatistic.totalNumberOfIndexes = 0;
        tokenStatistic.totalNumberOfActiveIndexes = 0;
        tokenStatistic.totalSubscriptions = 0;
        tokenStatistic.totalApprovedSubscriptions = 0;
        tokenStatistic.totalOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountTransferred = BIG_INT_ZERO;
        tokenStatistic.totalAmountDistributed = BIG_INT_ZERO;
        tokenStatistic.token = tokenId;
    }
    return tokenStatistic as TokenStatistic;
}

export function updateAggregateIDASubscriptionsData(
    accountId: string,
    tokenId: string,
    subscriptionExists: boolean,
    isDeletingSubscription: boolean,
    isApproving: boolean,
    lastModified: BigInt
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId,
        lastModified
    );
    let tokenStatistic = getOrInitTokenStatistic(tokenId, lastModified);
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
    accountTokenSnapshot.updatedAt = lastModified;

    // update tokenStatistic Subscriber data
    tokenStatistic.totalSubscriptions =
        tokenStatistic.totalSubscriptions + totalSubscriptionsDelta;
    tokenStatistic.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    tokenStatistic.updatedAt = lastModified;

    accountTokenSnapshot.save();
    tokenStatistic.save();
}

/**
 * Updates the balance property on the ATS entity.
 * Note: ATS = AccountTokenSnapshot
 * @param accountId
 * @param tokenId
 * @param lastModified
 */
export function updateATSBalance(
    accountId: string,
    tokenId: string,
    lastModified: BigInt
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId,
        lastModified
    );
    log.info("Token updateBalance: {}", [tokenId]);
    let superTokenContract = SuperToken.bind(Address.fromString(tokenId));
    let newBalance = superTokenContract.balanceOf(
        Address.fromString(accountId)
    );
    accountTokenSnapshot.balance = newBalance;
    accountTokenSnapshot.updatedAt = lastModified;
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
 * @param lastModified
 */
export function updateATSFlowRates(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt,
    lastModified: BigInt
): void {
    let senderATS = getOrInitAccountTokenSnapshot(
        senderId,
        tokenId,
        lastModified
    );
    let receiverATS = getOrInitAccountTokenSnapshot(
        receiverId,
        tokenId,
        lastModified
    );

    senderATS.totalNetFlowRate =
        senderATS.totalNetFlowRate.minus(flowRateDelta);
    senderATS.totalOutflowRate = senderATS.totalOutflowRate.plus(flowRateDelta);
    senderATS.updatedAt = lastModified;
    receiverATS.totalNetFlowRate =
        receiverATS.totalNetFlowRate.plus(flowRateDelta);
    receiverATS.totalInflowRate =
        receiverATS.totalInflowRate.plus(flowRateDelta);
    receiverATS.updatedAt = lastModified;

    senderATS.save();
    receiverATS.save();
}

export function updateAggregateEntitiesStreamData(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    lastModified: BigInt,
    amountStreamedSinceLastUpdate: BigInt
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenId, lastModified);
    let totalNumberOfStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    tokenStatistic.totalOutflowRate =
        tokenStatistic.totalOutflowRate.plus(flowRateDelta);
    tokenStatistic.totalNumberOfActiveStreams =
        tokenStatistic.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    tokenStatistic.totalAmountStreamedUntilUpdatedAt =
        tokenStatistic.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdate
        );
    tokenStatistic.updatedAt = lastModified;

    let senderATS = getOrInitAccountTokenSnapshot(
        senderId,
        tokenId,
        lastModified
    );
    let receiverATS = getOrInitAccountTokenSnapshot(
        receiverId,
        tokenId,
        lastModified
    );
    senderATS.totalNumberOfActiveStreams =
        senderATS.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    senderATS.updatedAt = lastModified;
    senderATS.totalAmountStreamedUntilUpdatedAt =
        senderATS.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdate
        );
    receiverATS.totalNumberOfActiveStreams =
        receiverATS.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    receiverATS.updatedAt = lastModified;

    if (isDelete) {
        tokenStatistic.totalNumberOfClosedStreams =
            tokenStatistic.totalNumberOfClosedStreams + 1;

        senderATS.totalNumberOfClosedStreams =
            senderATS.totalNumberOfClosedStreams + 1;
        receiverATS.totalNumberOfClosedStreams =
            receiverATS.totalNumberOfClosedStreams + 1;
    }

    tokenStatistic.save();
    senderATS.save();
    receiverATS.save();
}

// Get Aggregate ID functions
function getAccountTokenSnapshotID(accountId: string, tokenId: string): string {
    return accountId.concat("-").concat(tokenId);
}

export function updateAggregateEntitiesTransferData(
    transferAccountId: string,
    tokenId: string,
    currentTimestamp: BigInt,
    value: BigInt
): void {
    let fromAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
        transferAccountId,
        tokenId,
        currentTimestamp
    );
    fromAccountTokenSnapshot.totalAmountTransferred =
        fromAccountTokenSnapshot.totalAmountTransferred.plus(value);
    fromAccountTokenSnapshot.updatedAt = currentTimestamp;
    fromAccountTokenSnapshot.save();

    let tokenStatistic = getOrInitTokenStatistic(tokenId, currentTimestamp);
    tokenStatistic.totalAmountTransferred =
        tokenStatistic.totalAmountTransferred.plus(value);
    tokenStatistic.save();
}
