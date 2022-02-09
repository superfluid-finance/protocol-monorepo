import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { ISuperfluid as Superfluid } from "../generated/Host/ISuperfluid";
import {
    Account,
    AccountTokenSnapshot,
    Index,
    IndexSubscription,
    Stream,
    StreamRevision,
    Token,
    TokenStatistic,
} from "../generated/schema";
import {
    BIG_INT_ZERO,
    getAccountTokenSnapshotID,
    getAmountStreamedSinceLastUpdatedAt,
    getIndexID,
    getIsListedToken,
    getStreamID,
    getStreamRevisionPrefix,
    getSubscriptionID,
    getTokenInfoAndReturn,
    updateTotalSupplyForNativeSuperToken,
    streamRevisionExists,
    ZERO_ADDRESS,
} from "./utils";
import { SuperToken as SuperTokenTemplate } from "../generated/templates";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { getHostAddress, getResolverAddress } from "./addresses";

/**************************************************************************
 * HOL initializer functions
 *************************************************************************/
/**
 * Gets the Account entity with id or creates one with it. updatedAt is
 * updated each time any data associated with the user is updated.
 * @param accountAddress
 * @param block
 * @returns Account
 */
export function getOrInitAccount(
    accountAddress: Address,
    block: ethereum.Block
): Account {
    let account = Account.load(accountAddress.toHex());
    let hostAddress = getHostAddress();

    // filter out 0 address accounts
    if (accountAddress.equals(new Address(0))) {
        return account as Account;
    }

    let currentTimestamp = block.timestamp;
    if (account == null) {
        let hostContract = Superfluid.bind(hostAddress);
        let appManifestResult = hostContract.try_getAppManifest(accountAddress);
        account = new Account(accountAddress.toHex());
        account.createdAtTimestamp = currentTimestamp;
        account.createdAtBlockNumber = block.number;
        account.updatedAtTimestamp = currentTimestamp;
        account.updatedAtBlockNumber = block.number;
        if (appManifestResult.reverted) {
            account.isSuperApp = false;
        } else {
            account.isSuperApp = appManifestResult.value.value0;
        }
        account.save();
    }
    return account as Account;
}

/**
 * Creates a HOL Token (SuperToken) entity if non exists.
 * We also create token stats in here if it doesn't exist yet.
 * @param tokenAddress
 * @param block
 * @returns
 */
export function getOrInitSuperToken(
    tokenAddress: Address,
    block: ethereum.Block
): Token {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    let currentTimestamp = block.timestamp;
    let resolverAddress = getResolverAddress();

    if (tokenId == ZERO_ADDRESS.toHex()) {
        return token as Token;
    }

    if (token == null) {
        token = new Token(tokenId);
        token.createdAtTimestamp = currentTimestamp;
        token.createdAtBlockNumber = block.number;
        token.isSuperToken = true;
        token = getTokenInfoAndReturn(token as Token, tokenAddress);
        token = getIsListedToken(token as Token, tokenAddress, resolverAddress);
        let underlyingAddress = token.underlyingAddress;
        token.underlyingToken = underlyingAddress.toHexString();

        token.save();

        // Note: we initalize and create tokenStatistic whenever we create a
        // token as well.
        let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
        tokenStatistic = updateTotalSupplyForNativeSuperToken(
            token,
            tokenStatistic,
            tokenAddress
        );
        tokenStatistic.save();

        // Note: this is necessary otherwise we will not be able to capture
        // template data source events.
        SuperTokenTemplate.create(tokenAddress);

        // If the token has an underlying ERC20, we create a token entity for it.
        let underlyingToken = Token.load(token.underlyingAddress.toHex());
        if (
            underlyingAddress.notEqual(new Address(0)) &&
            underlyingToken == null
        ) {
            let address = Address.fromString(underlyingAddress.toHexString());
            getOrInitToken(address, block);
        }

        return token as Token;
    }

    // // we must handle the case when the native token hasn't been initialized
    // // there is no name/symbol, but this may occur later
    if (token.name.length == 0 || token.symbol.length == 0) {
        token = getTokenInfoAndReturn(token as Token, tokenAddress);
        token.save();
    }

    if (token.isListed == false) {
        token = getIsListedToken(token as Token, tokenAddress, resolverAddress);
        token.save();
    }

    return token as Token;
}

/**
 * Create a token entity for regular ERC20 tokens.
 * These are the underlying tokens for
 * @param tokenAddress
 * @param currentTimestamp
 */
export function getOrInitToken(
    tokenAddress: Address,
    block: ethereum.Block
): void {
    let tokenId = tokenAddress.toHex();
    let token = new Token(tokenId);

    if (tokenId == ZERO_ADDRESS.toHex()) {
        return;
    }

    token.createdAtTimestamp = block.timestamp;
    token.createdAtBlockNumber = block.number;
    token.isSuperToken = false;
    token.isListed = false;
    token = getTokenInfoAndReturn(token as Token, tokenAddress);
    token.save();
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
        streamRevision.periodRevisionIndex = 0;
    }
    return streamRevision as StreamRevision;
}

/**
 * Gets or initializes a Stream, always sets the updatedAt.
 * @param senderAddress
 * @param receiverAddress
 * @param tokenAddress
 * @param block
 * @returns
 */
export function getOrInitStream(
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block
): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(senderAddress, block);
    getOrInitAccount(receiverAddress, block);

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
        stream.createdAtTimestamp = currentTimestamp;
        stream.createdAtBlockNumber = block.number;
        stream.token = tokenAddress.toHex();
        stream.sender = senderAddress.toHex();
        stream.receiver = receiverAddress.toHex();
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);
        stream.updatedAtTimestamp = currentTimestamp;
        stream.updatedAtBlockNumber = block.number;

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        // also handles the fact that custom super tokens are
        // initialized after event is first initialized
        getOrInitSuperToken(tokenAddress, block);
    }
    return stream as Stream;
}

/**
 * Gets or initializes an Index, always sets the updatedAt.
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param block
 * @param indexCreatedId
 * @returns
 */
export function getOrInitIndex(
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    block: ethereum.Block,
    indexCreatedId: string
): Index {
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let index = Index.load(indexEntityId);
    let currentTimestamp = block.timestamp;
    if (index == null) {
        let publisherId = publisherAddress.toHex();
        let tokenId = tokenAddress.toHex();
        index = new Index(indexEntityId);
        index.createdAtTimestamp = currentTimestamp;
        index.createdAtBlockNumber = block.number;
        index.indexId = indexId;
        index.indexValue = BIG_INT_ZERO;
        index.totalSubscriptionsWithUnits = 0;
        index.totalUnitsPending = BIG_INT_ZERO;
        index.totalUnitsApproved = BIG_INT_ZERO;
        index.totalUnits = BIG_INT_ZERO;
        index.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        index.token = tokenId;
        index.publisher = publisherId;
        index.indexCreatedEvent = indexCreatedId;

        getOrInitAccount(publisherAddress, block);

        // NOTE: we must check if token exists and create here
        // if not. for SETH tokens (e.g. ETH, MATIC, xDAI)
        getOrInitSuperToken(tokenAddress, block);
    }
    index.updatedAtTimestamp = currentTimestamp;
    index.updatedAtBlockNumber = block.number;
    return index as Index;
}

/**
 * Gets or initializes a Subscription, always sets the updatedAt.
 * @param hostAddress
 * @param subscriberAddress
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param block
 * @returns subscription
 */
export function getOrInitSubscription(
    subscriberAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    block: ethereum.Block
): IndexSubscription {
    let subscriptionId = getSubscriptionID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscription = IndexSubscription.load(subscriptionId);
    let indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    let currentTimestamp = block.timestamp;

    if (subscription == null) {
        let index = getOrInitIndex(
            publisherAddress,
            tokenAddress,
            indexId,
            block,
            ""
        );

        let subscriberId = subscriberAddress.toHex();
        subscription = new IndexSubscription(subscriptionId);
        subscription.createdAtTimestamp = currentTimestamp;
        subscription.createdAtBlockNumber = block.number;
        subscription.subscriber = subscriberId;
        subscription.approved = false;
        subscription.units = BIG_INT_ZERO;
        subscription.totalAmountReceivedUntilUpdatedAt = BIG_INT_ZERO;
        subscription.indexValueUntilUpdatedAt = index.indexValue;
        subscription.index = indexEntityId;

        getOrInitAccount(subscriberAddress, block);
    }
    subscription.updatedAtTimestamp = currentTimestamp;
    subscription.updatedAtBlockNumber = block.number;
    return subscription as IndexSubscription;
}

/**************************************************************************
 * Aggregate initializer functions
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
        accountTokenSnapshot.updatedAtBlockNumber = block.number;
        accountTokenSnapshot.totalNumberOfActiveStreams = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalSubscriptionsWithUnits = 0;
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
        tokenStatistic.updatedAtBlockNumber = block.number;
        tokenStatistic.totalNumberOfActiveStreams = 0;
        tokenStatistic.totalNumberOfClosedStreams = 0;
        tokenStatistic.totalNumberOfIndexes = 0;
        tokenStatistic.totalNumberOfActiveIndexes = 0;
        tokenStatistic.totalSubscriptionsWithUnits = 0;
        tokenStatistic.totalApprovedSubscriptions = 0;
        tokenStatistic.totalOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountTransferredUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalSupply = BIG_INT_ZERO;
        tokenStatistic.token = tokenId;
    }
    return tokenStatistic as TokenStatistic;
}

/**************************************************************************
 * HOL Entities Updater functions
 *************************************************************************/

/**
 * Updates the Account entities updatedAt property.
 * @param hostAddress
 * @param accountAddress
 * @param block
 */
export function updateAccountUpdatedAt(
    accountAddress: Address,
    block: ethereum.Block
): void {
    // filter out 0 address accounts
    if (accountAddress.equals(new Address(0))) {
        return;
    }
    let account = getOrInitAccount(accountAddress, block);
    account.updatedAtTimestamp = block.timestamp;
    account.updatedAtBlockNumber = block.number;
    account.save();
}

/**************************************************************************
 * Aggregate Entities Updater functions
 *************************************************************************/

/**
 * Updates ATS and TokenStats IDA Subscriptions data.
 * @param accountId
 * @param tokenId
 * @param subscriptionWithUnitsExists
 * @param subscriptionApproved
 * @param isIncrementingSubWithUnits
 * @param isRevokingSubscription
 * @param isDeletingSubscription
 * @param isApproving
 * @param block
 */
export function updateAggregateIDASubscriptionsData(
    accountId: string,
    tokenId: string,
    subscriptionWithUnitsExists: boolean,
    subscriptionApproved: boolean,
    isIncrementingSubWithUnits: boolean,
    isRevokingSubscription: boolean,
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
    let totalSubscriptionWithUnitsDelta =
        isDeletingSubscription && subscriptionWithUnitsExists
            ? -1
            : isIncrementingSubWithUnits && !subscriptionWithUnitsExists
            ? 1
            : 0;
    let totalApprovedSubscriptionsDelta = isApproving
        ? 1
        : isRevokingSubscription && subscriptionApproved
        ? -1
        : 0;

    // update ATS Subscription data
    accountTokenSnapshot.totalSubscriptionsWithUnits =
        accountTokenSnapshot.totalSubscriptionsWithUnits +
        totalSubscriptionWithUnitsDelta;
    accountTokenSnapshot.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlockNumber = block.number;

    // update tokenStatistic Subscription data
    tokenStatistic.totalSubscriptionsWithUnits =
        tokenStatistic.totalSubscriptionsWithUnits +
        totalSubscriptionWithUnitsDelta;
    tokenStatistic.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    tokenStatistic.updatedAtTimestamp = block.timestamp;
    tokenStatistic.updatedAtBlockNumber = block.number;

    accountTokenSnapshot.save();
    tokenStatistic.save();
}

/**
 * Updates the balance property on the ATS entity.
 * Also updates the updatedAt time.
 * Note: ATS = AccountTokenSnapshot
 * @param accountId
 * @param tokenId
 * @param block
 */
function updateATSBalanceAndUpdatedAt(
    accountTokenSnapshot: AccountTokenSnapshot,
    block: ethereum.Block
): AccountTokenSnapshot {
    let superTokenContract = SuperToken.bind(
        Address.fromString(accountTokenSnapshot.token)
    );
    let newBalanceResult = superTokenContract.try_realtimeBalanceOf(
        Address.fromString(accountTokenSnapshot.account),
        block.timestamp
    );
    if (!newBalanceResult.reverted) {
        accountTokenSnapshot.balanceUntilUpdatedAt =
            newBalanceResult.value.value0;
    }
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlockNumber = block.number;
    return accountTokenSnapshot as AccountTokenSnapshot;
}

/**
 * Updates the amount streamed, balance until updated at for the AccountTokenSnapshot
 * entity and also updates the updatedAt property on the account entity.
 * @dev Must call before updatedAt is updated.
 * @param accountId
 * @param tokenId
 * @param block
 */
export function updateATSStreamedAndBalanceUntilUpdatedAt(
    accountId: string,
    tokenId: string,
    block: ethereum.Block
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountId,
        tokenId,
        block
    );
    let amountStreamedSinceLastUpdatedAt = getAmountStreamedSinceLastUpdatedAt(
        block.timestamp,
        accountTokenSnapshot.updatedAtTimestamp,
        accountTokenSnapshot.totalOutflowRate
    );

    // update the totalStreamedUntilUpdatedAt
    accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdatedAt
        );

    // update the balance via external call and saves the entity
    // NOTE: this is the main culprit which slows things down currently
    accountTokenSnapshot = updateATSBalanceAndUpdatedAt(
        accountTokenSnapshot,
        block
    );
    accountTokenSnapshot.save();

    // update the updatedAt property of the account that just made an update
    updateAccountUpdatedAt(Address.fromString(accountId), block);
}

export function updateTokenStatsStreamedUntilUpdatedAt(
    tokenId: string,
    block: ethereum.Block
): void {
    let tokenStats = getOrInitTokenStatistic(tokenId, block);
    let amountStreamedSinceLastUpdatedAt = getAmountStreamedSinceLastUpdatedAt(
        block.timestamp,
        tokenStats.updatedAtTimestamp,
        tokenStats.totalOutflowRate
    );
    tokenStats.totalAmountStreamedUntilUpdatedAt =
        tokenStats.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdatedAt
        );
    tokenStats.save();
}

/**
 * Updates TokenStatistic and AccountTokenSnapshot countable stream
 * data. Must be called after updating streamed amount data for the
 * AccountTokenSnapshot entities.
 * @param senderId
 * @param receiverId
 * @param tokenId
 * @param newFlowRate
 * @param flowRateDelta
 * @param isCreate
 * @param isDelete
 * @param block
 */
export function updateAggregateEntitiesStreamData(
    senderId: string,
    receiverId: string,
    tokenId: string,
    newFlowRate: BigInt,
    flowRateDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    block: ethereum.Block
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
    let totalNumberOfActiveStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    let totalNumberOfClosedStreamsDelta = isDelete ? 1 : 0;
    let tokenStatsAmountStreamedSinceLastUpdate =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            tokenStatistic.updatedAtTimestamp,
            tokenStatistic.totalOutflowRate
        );

    // the outflow rate should never go below 0.
    tokenStatistic.totalOutflowRate = tokenStatistic.totalOutflowRate
        .plus(flowRateDelta)
        .lt(BIG_INT_ZERO)
        ? newFlowRate
        : tokenStatistic.totalOutflowRate.plus(flowRateDelta);
    tokenStatistic.totalNumberOfActiveStreams =
        tokenStatistic.totalNumberOfActiveStreams +
        totalNumberOfActiveStreamsDelta;
    tokenStatistic.totalNumberOfClosedStreams =
        tokenStatistic.totalNumberOfClosedStreams +
        totalNumberOfClosedStreamsDelta;
    tokenStatistic.totalAmountStreamedUntilUpdatedAt =
        tokenStatistic.totalAmountStreamedUntilUpdatedAt.plus(
            tokenStatsAmountStreamedSinceLastUpdate
        );
    tokenStatistic.updatedAtTimestamp = block.timestamp;
    tokenStatistic.updatedAtBlockNumber = block.number;

    let senderATS = getOrInitAccountTokenSnapshot(senderId, tokenId, block);
    senderATS.totalNetFlowRate =
        senderATS.totalNetFlowRate.minus(flowRateDelta);
    // the outflow rate should never go below 0.
    senderATS.totalOutflowRate = senderATS.totalOutflowRate
        .plus(flowRateDelta)
        .lt(BIG_INT_ZERO)
        ? newFlowRate
        : senderATS.totalOutflowRate.plus(flowRateDelta);
    senderATS.totalNumberOfActiveStreams =
        senderATS.totalNumberOfActiveStreams + totalNumberOfActiveStreamsDelta;
    senderATS.totalNumberOfClosedStreams =
        senderATS.totalNumberOfClosedStreams + totalNumberOfClosedStreamsDelta;

    let receiverATS = getOrInitAccountTokenSnapshot(receiverId, tokenId, block);
    receiverATS.totalNetFlowRate =
        receiverATS.totalNetFlowRate.plus(flowRateDelta);
    // the inflow rate should never go below 0.
    receiverATS.totalInflowRate = receiverATS.totalInflowRate
        .plus(flowRateDelta)
        .lt(BIG_INT_ZERO)
        ? newFlowRate
        : receiverATS.totalInflowRate.plus(flowRateDelta);
    receiverATS.totalNumberOfActiveStreams =
        receiverATS.totalNumberOfActiveStreams +
        totalNumberOfActiveStreamsDelta;
    receiverATS.totalNumberOfClosedStreams =
        receiverATS.totalNumberOfClosedStreams +
        totalNumberOfClosedStreamsDelta;

    tokenStatistic.save();
    senderATS.save();
    receiverATS.save();
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
    fromAccountTokenSnapshot.updatedAtBlockNumber = block.number;
    fromAccountTokenSnapshot.save();

    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
    tokenStatistic.totalAmountTransferredUntilUpdatedAt =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt.plus(value);
    tokenStatistic.save();
}
