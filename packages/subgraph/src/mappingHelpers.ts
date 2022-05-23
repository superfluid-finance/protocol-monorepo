import {Address, BigInt, ethereum} from "@graphprotocol/graph-ts";
import {ISuperfluid as Superfluid} from "../generated/Host/ISuperfluid";
import {
    Account,
    AccountTokenSnapshot,
    AccountTokenSnapshotLog,
    FlowOperator,
    Index,
    IndexSubscription,
    Stream,
    StreamRevision,
    Token,
    TokenStatistic,
} from "../generated/schema";
import {
    BIG_INT_ZERO,
    createLogID,
    calculateMaybeCriticalAtTimestamp,
    getAccountTokenSnapshotID,
    getAmountStreamedSinceLastUpdatedAt,
    getFlowOperatorID,
    getIndexID,
    getIsListedToken,
    getOrder,
    getStreamID,
    getStreamRevisionID,
    getSubscriptionID,
    getTokenInfoAndReturn,
    streamRevisionExists,
    updateTotalSupplyForNativeSuperToken,
    ZERO_ADDRESS,
} from "./utils";
import {SuperToken as SuperTokenTemplate} from "../generated/templates";
import {ISuperToken as SuperToken} from "../generated/templates/SuperToken/ISuperToken";
import {getHostAddress, getResolverAddress} from "./addresses";
import {FlowUpdated} from "../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";

/**************************************************************************
 * HOL initializer functions
 *************************************************************************/
/**
 * NOTE: ALWAYS CHECK ACCOUNT IS NOT ZERO_ADDRESS BEFORE CALLING THIS FUNCTION
 * Gets the Account entity with id or creates one with it. updatedAt is
 * updated each time any data associated with the user is updated.
 */
export function getOrInitAccount(
    accountAddress: Address,
    block: ethereum.Block
): Account {
    let account = Account.load(accountAddress.toHex());
    let hostAddress = getHostAddress();

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
 */
export function getOrInitSuperToken(
    tokenAddress: Address,
    block: ethereum.Block
): Token {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    if (tokenAddress.equals(ZERO_ADDRESS)) {
        return token as Token;
    }

    let currentTimestamp = block.timestamp;
    let resolverAddress = getResolverAddress();

    if (token == null) {
        // Note: this is necessary otherwise we will not be able to capture
        // template data source events.
        SuperTokenTemplate.create(tokenAddress);

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
        let tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
        tokenStatistic = updateTotalSupplyForNativeSuperToken(
            token,
            tokenStatistic,
            tokenAddress
        );
        tokenStatistic.save();

        // If the token has an underlying ERC20, we create a token entity for it.
        let underlyingToken = Token.load(token.underlyingAddress.toHex());
        if (
            underlyingAddress.notEqual(ZERO_ADDRESS) &&
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
 */
export function getOrInitToken(
    tokenAddress: Address,
    block: ethereum.Block
): void {
    let tokenId = tokenAddress.toHex();
    let token = new Token(tokenId);

    if (tokenAddress.equals(ZERO_ADDRESS)) {
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
 */
export function getOrInitStreamRevision(
    senderAddress: Address,
    recipientAddress: Address,
    tokenAddress: Address
): StreamRevision {
    let streamRevisionId = getStreamRevisionID(
        senderAddress,
        recipientAddress,
        tokenAddress
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
 */
export function getOrInitStream(event: FlowUpdated): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(event.params.sender, event.block);
    getOrInitAccount(event.params.receiver, event.block);

    // Create a streamRevision entity for this stream if one doesn't exist.
    let streamRevision = getOrInitStreamRevision(
        event.params.sender,
        event.params.receiver,
        event.params.token
    );
    let currentTimestamp = event.block.timestamp;
    if (
        !streamRevisionExists(
            getStreamRevisionID(
                event.params.sender,
                event.params.receiver,
                event.params.token
            )
        )
    ) {
        streamRevision.save();
    }
    let id = getStreamID(
        event.params.sender,
        event.params.receiver,
        event.params.token,
        streamRevision.revisionIndex
    );
    let stream = Stream.load(id);
    if (stream == null) {
        stream = new Stream(id);
        stream.createdAtTimestamp = currentTimestamp;
        stream.createdAtBlockNumber = event.block.number;
        stream.token = event.params.token.toHex();
        stream.sender = event.params.sender.toHex();
        stream.receiver = event.params.receiver.toHex();
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);
        stream.updatedAtTimestamp = currentTimestamp;
        stream.updatedAtBlockNumber = event.block.number;

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        // also handles the fact that custom super tokens are
        // initialized after event is first initialized
        getOrInitSuperToken(event.params.token, event.block);
        stream.save();
    }
    return stream as Stream;
}

export function getOrInitFlowOperator(
    block: ethereum.Block,
    flowOperatorAddress: Address,
    tokenAddress: Address,
    senderAddress: Address
): FlowOperator {
    let flowOperatorId = getFlowOperatorID(
        flowOperatorAddress,
        tokenAddress,
        senderAddress
    );
    let flowOperatorEntity = FlowOperator.load(flowOperatorId);
    let currentTimestamp = block.timestamp;
    if (flowOperatorEntity == null) {
        flowOperatorEntity = new FlowOperator(flowOperatorId);
        flowOperatorEntity.createdAtBlockNumber = block.number;
        flowOperatorEntity.createdAtTimestamp = currentTimestamp;
        flowOperatorEntity.permissions = 0;
        flowOperatorEntity.flowRateAllowanceGranted = BigInt.fromI32(0);
        flowOperatorEntity.flowRateAllowanceRemaining = BigInt.fromI32(0);
        flowOperatorEntity.sender = senderAddress.toHex();
        flowOperatorEntity.token = tokenAddress.toHex();
        flowOperatorEntity.accountTokenSnapshot = getAccountTokenSnapshotID(
            senderAddress,
            tokenAddress
        );
        flowOperatorEntity.updatedAtBlockNumber = block.number;
        flowOperatorEntity.updatedAtTimestamp = currentTimestamp;
        flowOperatorEntity.save();
    }
    flowOperatorEntity.updatedAtBlockNumber = block.number;
    flowOperatorEntity.updatedAtTimestamp = currentTimestamp;

    return flowOperatorEntity;
}

/**
 * Gets or initializes an Index, always sets the updatedAt.
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

        index.updatedAtTimestamp = currentTimestamp;
        index.updatedAtBlockNumber = block.number;
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
        index.save();

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
        subscription.updatedAtTimestamp = currentTimestamp;
        subscription.updatedAtBlockNumber = block.number;
        subscription.save();

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
    accountAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block
): AccountTokenSnapshot {
    let atsId = getAccountTokenSnapshotID(accountAddress, tokenAddress);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);

    if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
        accountTokenSnapshot.updatedAtBlockNumber = block.number;
        accountTokenSnapshot.totalNumberOfActiveStreams = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalSubscriptionsWithUnits = 0;
        accountTokenSnapshot.isLiquidationEstimateOptimistic = false;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.balanceUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalInflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalDeposit = BIG_INT_ZERO;
        accountTokenSnapshot.maybeCriticalAtTimestamp = BIG_INT_ZERO;
        accountTokenSnapshot.account = accountAddress.toHex();
        accountTokenSnapshot.token = tokenAddress.toHex();
        accountTokenSnapshot.save();
    }
    return accountTokenSnapshot as AccountTokenSnapshot;
}

export function getOrInitTokenStatistic(
    tokenAddress: Address,
    block: ethereum.Block
): TokenStatistic {
    const tokenId = tokenAddress.toHex();
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
        tokenStatistic.totalDeposit = BIG_INT_ZERO;
        tokenStatistic.token = tokenId;
        tokenStatistic.save();
    }
    return tokenStatistic as TokenStatistic;
}

/**************************************************************************
 * HOL Entities Updater functions
 *************************************************************************/

/**
 * Updates the Account entities updatedAt property.
 */
export function updateAccountUpdatedAt(
    accountAddress: Address,
    block: ethereum.Block
): void {
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
 */
export function updateAggregateIDASubscriptionsData(
    accountAddress: Address,
    tokenAddress: Address,
    subscriptionWithUnitsExists: boolean,
    subscriptionApproved: boolean,
    isIncrementingSubWithUnits: boolean,
    isRevokingSubscription: boolean,
    isDeletingSubscription: boolean,
    isApproving: boolean,
    block: ethereum.Block
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
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
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
        block
    );

    accountTokenSnapshot.totalSubscriptionsWithUnits =
        accountTokenSnapshot.totalSubscriptionsWithUnits +
        totalSubscriptionWithUnitsDelta;
    accountTokenSnapshot.isLiquidationEstimateOptimistic = accountTokenSnapshot.totalSubscriptionsWithUnits > 0;
    accountTokenSnapshot.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlockNumber = block.number;

    accountTokenSnapshot.save();

    // update tokenStatistic Subscription data
    tokenStatistic.totalSubscriptionsWithUnits =
        tokenStatistic.totalSubscriptionsWithUnits +
        totalSubscriptionWithUnitsDelta;
    accountTokenSnapshot.isLiquidationEstimateOptimistic = accountTokenSnapshot.totalSubscriptionsWithUnits > 0;
    tokenStatistic.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions +
        totalApprovedSubscriptionsDelta;
    tokenStatistic.updatedAtTimestamp = block.timestamp;
    tokenStatistic.updatedAtBlockNumber = block.number;

    tokenStatistic.save();
}

/**
 * Updates the balance property on the ATS entity.
 * Also updates the updatedAt time.
 * Note: ATS = AccountTokenSnapshot
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

    accountTokenSnapshot.maybeCriticalAtTimestamp = calculateMaybeCriticalAtTimestamp(
        accountTokenSnapshot.updatedAtTimestamp,
        accountTokenSnapshot.balanceUntilUpdatedAt,
        accountTokenSnapshot.totalDeposit,
        accountTokenSnapshot.totalNetFlowRate
    );

    accountTokenSnapshot.save();
    return accountTokenSnapshot as AccountTokenSnapshot;
}

/**
 * Updates the amount streamed, balance until updated at for the AccountTokenSnapshot
 * entity and also updates the updatedAt property on the account entity.
 * @dev Must call before updatedAt is updated.
 */
export function updateATSStreamedAndBalanceUntilUpdatedAt(
    accountAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
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
    updateAccountUpdatedAt(accountAddress, block);
}

export function updateTokenStatsStreamedUntilUpdatedAt(
    tokenAddress: Address,
    block: ethereum.Block
): void {
    let tokenStats = getOrInitTokenStatistic(tokenAddress, block);
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
 */
export function updateAggregateEntitiesStreamData(
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address,
    newFlowRate: BigInt,
    flowRateDelta: BigInt,
    depositDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    block: ethereum.Block
): void {
    let tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
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

    tokenStatistic.totalDeposit =
        tokenStatistic.totalDeposit.plus(depositDelta);

    let senderATS = getOrInitAccountTokenSnapshot(
        senderAddress,
        tokenAddress,
        block
    );
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
    senderATS.totalDeposit = senderATS.totalDeposit.plus(depositDelta);
    senderATS.maybeCriticalAtTimestamp = calculateMaybeCriticalAtTimestamp(
        senderATS.updatedAtTimestamp,
        senderATS.balanceUntilUpdatedAt,
        senderATS.totalDeposit,
        senderATS.totalNetFlowRate
    );

    let receiverATS = getOrInitAccountTokenSnapshot(
        receiverAddress,
        tokenAddress,
        block
    );
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

    receiverATS.maybeCriticalAtTimestamp = calculateMaybeCriticalAtTimestamp(
        receiverATS.updatedAtTimestamp,
        receiverATS.balanceUntilUpdatedAt,
        receiverATS.totalDeposit,
        receiverATS.totalNetFlowRate
    );
    receiverATS.save();

    tokenStatistic.save();
    senderATS.save();
}

export function updateAggregateEntitiesTransferData(
    fromAddress: Address,
    tokenAddress: Address,
    value: BigInt,
    block: ethereum.Block
): void {
    let fromAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
        fromAddress,
        tokenAddress,
        block
    );

    // NOTE: fromAccountTokenSnapshot won't exist if address is ZERO_ADDRESS
    fromAccountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
        fromAccountTokenSnapshot.totalAmountTransferredUntilUpdatedAt.plus(
            value
        );
    fromAccountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    fromAccountTokenSnapshot.updatedAtBlockNumber = block.number;
    fromAccountTokenSnapshot.save();

    let tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    tokenStatistic.totalAmountTransferredUntilUpdatedAt =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt.plus(value);
    tokenStatistic.save();
}

export function createAccountTokenSnapshotLogEntity(
    event: ethereum.Event,
    accountAddress: Address,
    tokenAddress: Address,
    eventName: string,
): void {
    if (accountAddress.equals(ZERO_ADDRESS)) {
        return;
    }
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(accountAddress, tokenAddress, event.block);
    // Transaction
    let atsLog = new AccountTokenSnapshotLog(createLogID("ATSLog", accountTokenSnapshot.id, event));
    atsLog.transactionHash = event.transaction.hash;
    atsLog.timestamp = event.block.timestamp;
    atsLog.order = getOrder(event.block.number, event.logIndex);
    atsLog.blockNumber = event.block.number;
    atsLog.logIndex = event.logIndex;
    atsLog.triggeredByEventName = eventName;
    // Account token snapshot state
    atsLog.totalNumberOfActiveStreams = accountTokenSnapshot.totalNumberOfActiveStreams;
    atsLog.totalNumberOfClosedStreams = accountTokenSnapshot.totalNumberOfClosedStreams;
    atsLog.totalSubscriptionsWithUnits = accountTokenSnapshot.totalSubscriptionsWithUnits;
    atsLog.totalApprovedSubscriptions = accountTokenSnapshot.totalApprovedSubscriptions;
    atsLog.balance = accountTokenSnapshot.balanceUntilUpdatedAt;
    atsLog.totalNetFlowRate = accountTokenSnapshot.totalNetFlowRate;
    atsLog.totalInflowRate = accountTokenSnapshot.totalInflowRate;
    atsLog.totalOutflowRate = accountTokenSnapshot.totalOutflowRate;
    atsLog.totalAmountStreamed = accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt;
    atsLog.totalAmountTransferred = accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt;
    atsLog.totalDeposit = accountTokenSnapshot.totalDeposit;
    atsLog.maybeCriticalAtTimestamp = accountTokenSnapshot.maybeCriticalAtTimestamp;
    atsLog.account = accountTokenSnapshot.account;
    atsLog.token = accountTokenSnapshot.token;
    atsLog.accountTokenSnapshot = accountTokenSnapshot.id;
    atsLog.save();
}
