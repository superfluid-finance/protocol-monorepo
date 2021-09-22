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

export let GOERLI_HOST_ADDRESS = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
export let KOVAN_HOST_ADDRESS = "0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3";
export let MATIC_HOST_ADDRESS = "0x3E14dC1b13c488a8d5D310918780c983bD5982E7";
export let MUMBAI_HOST_ADDRESS = "0xEB796bdb90fFA0f28255275e16936D25d3418603";
export let RINKEBY_HOST_ADDRESS = "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6";
export let ROPSTEN_HOST_ADDRESS = "0xF2B4E81ba39F5215Db2e05B2F66f482BB8e87FD2";
export let XDAI_HOST_ADDRESS = "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7";

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
 * Gets the Account entity with id or creates one with it. updatedAt is
 * updated each time any data associated with the user is updated.
 * @param hostAddress
 * @param accountAddress
 * @param block
 * @returns Account
 */
export function getOrInitAccount(
    hostAddress: Address,
    accountAddress: Address,
    block: ethereum.Block
): Account {
    let account = Account.load(accountAddress.toHex());
    let currentTimestamp = block.timestamp;
    if (account == null) {
        let hostContract = Superfluid.bind(hostAddress);
        let appManifestResult = hostContract.try_getAppManifest(accountAddress);
        account = new Account(accountAddress.toHex());
        account.createdAt = currentTimestamp;
        account.updatedAtTimestamp = currentTimestamp;
        account.updatedAtBlock = block.number;
        if (appManifestResult.reverted) {
            account.isSuperApp = false;
        } else {
            account.isSuperApp = appManifestResult.value.value0;
        }
        account.save();
    }
    return account as Account;
}

export function getTokenInfoAndReturn(
    token: Token,
    tokenAddress: Address
): Token {
    let tokenContract = SuperToken.bind(tokenAddress);
    let underlyingAddressResult = tokenContract.try_getUnderlyingToken();
    let nameResult = tokenContract.try_name();
    let symbolResult = tokenContract.try_symbol();
    token.underlyingAddress = underlyingAddressResult.reverted
        ? new Address(0)
        : underlyingAddressResult.value;
    token.name = nameResult.reverted ? "" : nameResult.value;
    token.symbol = symbolResult.reverted ? "" : symbolResult.value;
    return token;
}

/**
 * Creates a HOL Token (SuperToken) entity if non exists.
 * We also create token stats in here if it doesn't exist yet.
 * @param tokenAddress
 * @param block
 * @returns Token
 */
export function getOrInitToken(
    tokenAddress: Address,
    block: ethereum.Block
): Token {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    let currentTimestamp = block.timestamp;

    if (token == null) {
        token = new Token(tokenId);
        token.createdAt = currentTimestamp;
        token.updatedAtTimestamp = currentTimestamp;
        token.updatedAtBlock = block.number;
        token = getTokenInfoAndReturn(token as Token, tokenAddress);
        token.save();

        // Note: we initalize and create tokenStatistic whenever we create a
        // token as well.
        let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
        tokenStatistic.save();
        return token as Token;
    }

    // // we must handle the case when the native token hasn't been initialized
    // // there is no name/symbol, but this may occur later
    if (token.name.length == 0 || token.symbol.length == 0) {
        token = getTokenInfoAndReturn(token as Token, tokenAddress);
        token.save();
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
 * @param hostAddress
 * @param senderAddress
 * @param receiverAddress
 * @param tokenAddress
 * @param block
 * @returns Stream
 */
export function getOrInitStream(
    hostAddress: Address,
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block
): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(hostAddress, senderAddress, block);
    getOrInitAccount(hostAddress, receiverAddress, block);

    // Create a streamRevision entity for this stream if one doesn't exist.
    let streamRevision = getOrInitStreamRevision(
        senderAddress.toHex(),
        receiverAddress.toHex(),
        tokenAddress.toHex()
    );
    let currentTimestamp = block.timestamp;
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
        stream.createdAt = currentTimestamp;
        stream.token = tokenAddress.toHex();
        stream.sender = senderAddress.toHex();
        stream.receiver = receiverAddress.toHex();
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        // also handles the fact that custom super tokens are
        // initialized after event is first initialized
        getOrInitToken(tokenAddress, block);
    }
    stream.updatedAtTimestamp = currentTimestamp;
    stream.updatedAtBlock = block.number;
    return stream as Stream;
}

/**
 * Gets or initializes an Index, always sets the updatedAt.
 * @param hostAddress
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param block
 * @returns Index
 */
export function getOrInitIndex(
    hostAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    block: ethereum.Block
): Index {
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let index = Index.load(indexEntityId);
    let currentTimestamp = block.timestamp;
    if (index == null) {
        let publisherId = publisherAddress.toHex();
        let tokenId = tokenAddress.toHex();
        index = new Index(indexEntityId);
        index.createdAt = currentTimestamp;
        index.indexId = indexId;
        index.userData = new Bytes(0);
        index.oldIndexValue = BIG_INT_ZERO;
        index.newIndexValue = BIG_INT_ZERO;
        index.totalSubscribers = 0;
        index.totalUnitsPending = BIG_INT_ZERO;
        index.totalUnitsApproved = BIG_INT_ZERO;
        index.totalUnits = BIG_INT_ZERO;
        index.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        index.token = tokenId;
        index.publisher = publisherId;

        getOrInitAccount(hostAddress, publisherAddress, block);

        // NOTE: we must check if token exists and create here
        // if not. for SETH tokens (e.g. ETH, MATIC, xDAI)
        getOrInitToken(tokenAddress, block);
    }
    index.updatedAtTimestamp = currentTimestamp;
    index.updatedAtBlock = block.number;
    return index as Index;
}

/**
 * Gets or initializes a Subscriber, always sets the updatedAt.
 * @param hostAddress
 * @param subscriberAddress
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param block
 * @returns Subscriber
 */
export function getOrInitSubscriber(
    hostAddress: Address,
    subscriberAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    block: ethereum.Block
): Subscriber {
    let subscriberEntityId = getSubscriberID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscriber = Subscriber.load(subscriberEntityId);
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let currentTimestamp = block.timestamp;

    if (subscriber == null) {
        let index = getOrInitIndex(
            hostAddress,
            publisherAddress,
            tokenAddress,
            indexId,
            block
        );
        index.totalSubscribers = index.totalSubscribers + 1;
        index.save();

        let subscriberId = subscriberAddress.toHex();
        subscriber = new Subscriber(subscriberEntityId);
        subscriber.createdAt = currentTimestamp;
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

        getOrInitAccount(hostAddress, subscriberAddress, block);
    }
    subscriber.updatedAtTimestamp = currentTimestamp;
    subscriber.updatedAtBlock = block.number;
    return subscriber as Subscriber;
}

/**
 * Helper function which finds out whether a token has a valid host address.
 * If it does not, we should not create any HOL/events related to the token.
 * @param hostAddress
 * @param tokenAddress
 * @returns
 */
export function tokenHasValidHost(
    hostAddress: Address,
    tokenAddress: Address
): boolean {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    if (token == null) {
        let tokenContract = SuperToken.bind(tokenAddress);
        let tokenHostAddressResult = tokenContract.try_getHost();

        if (tokenHostAddressResult.reverted) {
            log.error("REVERTED GET HOST = {}", [tokenAddress.toHex()]);
            return false;
        }

        return tokenHostAddressResult.value.toHex() == hostAddress.toHex();
    }

    return true;
}

/**
 * Updates the Account entities updatedAt property.
 * @param hostAddress
 * @param accountAddress
 * @param block
 */
export function updateAccountUpdatedAt(
    hostAddress: Address,
    accountAddress: Address,
    block: ethereum.Block
): void {
    let account = getOrInitAccount(hostAddress, accountAddress, block);
    account.updatedAtTimestamp = block.timestamp;
    account.updatedAtBlock = block.number;
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
    block: ethereum.Block
): AccountTokenSnapshot {
    let atsId = getAccountTokenSnapshotID(accountId, tokenId);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);
    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
        accountTokenSnapshot.updatedAtBlock = block.number;
        accountTokenSnapshot.totalNumberOfActiveStreams = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalSubscriptions = 0;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.balanceUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalInflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.account = accountId;
        accountTokenSnapshot.token = tokenId;
    }
    return accountTokenSnapshot as AccountTokenSnapshot;
}

export function getOrInitTokenStatistic(
    tokenId: string,
    block: ethereum.Block
): TokenStatistic {
    let tokenStatistic = TokenStatistic.load(tokenId);
    if (tokenStatistic == null) {
        tokenStatistic = new TokenStatistic(tokenId);
        tokenStatistic.updatedAtTimestamp = block.timestamp;
        tokenStatistic.updatedAtBlock = block.number;
        tokenStatistic.totalNumberOfActiveStreams = 0;
        tokenStatistic.totalNumberOfClosedStreams = 0;
        tokenStatistic.totalNumberOfIndexes = 0;
        tokenStatistic.totalNumberOfActiveIndexes = 0;
        tokenStatistic.totalSubscriptions = 0;
        tokenStatistic.totalApprovedSubscriptions = 0;
        tokenStatistic.totalOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountTransferredUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
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
    block: ethereum.Block
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId,
        block
    );
    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
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
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlock = block.number;

    // update tokenStatistic Subscriber data
    tokenStatistic.totalSubscriptions =
        tokenStatistic.totalSubscriptions + totalSubscriptionsDelta;
    tokenStatistic.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    tokenStatistic.updatedAtTimestamp = block.timestamp;
    tokenStatistic.updatedAtBlock = block.number;

    accountTokenSnapshot.save();
    tokenStatistic.save();
}

/**
 * Updates the balance property on the ATS entity.
 * Note: ATS = AccountTokenSnapshot
 * @param accountId
 * @param tokenId
 * @param block
 */
export function updateATSBalance(
    accountId: string,
    tokenId: string,
    block: ethereum.Block
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId,
        block
    );
    let superTokenContract = SuperToken.bind(Address.fromString(tokenId));
    let newBalance = superTokenContract.balanceOf(
        Address.fromString(accountId)
    );
    accountTokenSnapshot.balanceUntilUpdatedAt = newBalance;
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlock = block.number;
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
 * @param block
 */
export function updateATSFlowRates(
    senderId: string,
    receiverId: string,
    tokenId: string,
    flowRateDelta: BigInt,
    block: ethereum.Block
): void {
    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId, block);
    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId, block);

    senderATS.totalNetFlowRate =
        senderATS.totalNetFlowRate.minus(flowRateDelta);
    senderATS.totalOutflowRate = senderATS.totalOutflowRate.plus(flowRateDelta);
    senderATS.updatedAtTimestamp = block.timestamp;
    senderATS.updatedAtBlock = block.number;
    receiverATS.totalNetFlowRate =
        receiverATS.totalNetFlowRate.plus(flowRateDelta);
    receiverATS.totalInflowRate =
        receiverATS.totalInflowRate.plus(flowRateDelta);
    receiverATS.updatedAtTimestamp = block.timestamp;
    receiverATS.updatedAtBlock = block.number;

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
    block: ethereum.Block,
    amountStreamedSinceLastUpdate: BigInt
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
    let totalNumberOfStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    tokenStatistic.totalOutflowRate =
        tokenStatistic.totalOutflowRate.plus(flowRateDelta);
    tokenStatistic.totalNumberOfActiveStreams =
        tokenStatistic.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    tokenStatistic.totalAmountStreamedUntilUpdatedAt =
        tokenStatistic.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdate
        );
    tokenStatistic.updatedAtTimestamp = block.timestamp;
    tokenStatistic.updatedAtBlock = block.number;

    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId, block);
    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId, block);
    senderATS.totalNumberOfActiveStreams =
        senderATS.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    senderATS.updatedAtTimestamp = block.timestamp;
    senderATS.updatedAtBlock = block.number;
    senderATS.totalAmountStreamedUntilUpdatedAt =
        senderATS.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdate
        );
    receiverATS.totalNumberOfActiveStreams =
        receiverATS.totalNumberOfActiveStreams + totalNumberOfStreamsDelta;
    receiverATS.updatedAtTimestamp = block.timestamp;
    receiverATS.updatedAtBlock = block.number;

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
    value: BigInt,
    block: ethereum.Block
): void {
    let fromAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
        transferAccountId,
        tokenId,
        block
    );
    fromAccountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
        fromAccountTokenSnapshot.totalAmountTransferredUntilUpdatedAt.plus(
            value
        );
    fromAccountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    fromAccountTokenSnapshot.updatedAtBlock = block.number;
    fromAccountTokenSnapshot.save();

    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
    tokenStatistic.totalAmountTransferredUntilUpdatedAt =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt.plus(value);
    tokenStatistic.save();
}
