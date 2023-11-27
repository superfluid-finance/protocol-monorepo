import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { ISuperfluid as Superfluid } from "../generated/Host/ISuperfluid";
import {
    Account,
    AccountTokenSnapshot,
    AccountTokenSnapshotLog,
    FlowOperator,
    Index,
    IndexSubscription,
    ResolverEntry,
    Stream,
    StreamRevision,
    Token,
    TokenGovernanceConfig,
    TokenStatistic,
    TokenStatisticLog,
} from "../generated/schema";
import {
    BIG_INT_ZERO,
    createLogID,
    calculateMaybeCriticalAtTimestamp,
    getAccountTokenSnapshotID,
    getAmountStreamedSinceLastUpdatedAt,
    getFlowOperatorID,
    getIndexID,
    getOrder,
    getStreamID,
    getStreamRevisionID,
    getSubscriptionID,
    getInitialTotalSupplyForSuperToken,
    ZERO_ADDRESS,
    handleTokenRPCCalls,
} from "./utils";
import { SuperToken as SuperTokenTemplate } from "../generated/templates";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import {
    getHostAddress,
    getNativeAssetSuperTokenAddress,
    getResolverAddress,
} from "./addresses";
import { FlowUpdated } from "../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";

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
    const hostAddress = getHostAddress();

    const currentTimestamp = block.timestamp;
    if (account == null) {
        const hostContract = Superfluid.bind(hostAddress);
        const appManifestResult =
            hostContract.try_getAppManifest(accountAddress);
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
    event: ethereum.Event,
    tokenAddress: Address,
    triggeredByEventName: string
): Token {
    const tokenId = tokenAddress.toHex();
    const block = event.block;
    let token = Token.load(tokenId);
    if (tokenAddress.equals(ZERO_ADDRESS)) {
        return token as Token;
    }

    const currentTimestamp = block.timestamp;
    const resolverAddress = getResolverAddress();

    if (token == null) {
        // Note: this is necessary otherwise we will not be able to capture
        // template data source events.
        SuperTokenTemplate.create(tokenAddress);

        token = new Token(tokenId);
        token.createdAtTimestamp = currentTimestamp;
        token.createdAtBlockNumber = block.number;
        token.isSuperToken = true;
        token.name = "";
        token.symbol = "";

        const nativeAssetSuperTokenAddress = getNativeAssetSuperTokenAddress();
        token.isNativeAssetSuperToken = tokenAddress.equals(
            nativeAssetSuperTokenAddress
        );

        token = handleTokenRPCCalls(token, resolverAddress);
        token.isListed = false;
        const underlyingAddress = token.underlyingAddress;
        token.underlyingToken = underlyingAddress.toHexString();

        token.save();

        // Note: we initialize and create tokenStatistic whenever we create a
        // token as well.
        let tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
        tokenStatistic = getInitialTotalSupplyForSuperToken(
            tokenStatistic,
            tokenAddress
        );
        tokenStatistic.save();

        // Per our TokenStatistic Invariant: whenever we create TokenStatistic, we must create TokenStatisticLog
        _createTokenStatisticLogEntity(
            event,
            tokenAddress,
            triggeredByEventName
        );

        // If the token has an underlying ERC20, we create a token entity for it.
        if (
            underlyingAddress.notEqual(ZERO_ADDRESS) &&
            Token.load(token.underlyingAddress.toHex()) == null
        ) {
            let address = Address.fromString(underlyingAddress.toHexString());
            getOrInitToken(address, block, resolverAddress);
        }
        return token as Token;
    }

    // @note - this is currently being called every single time to handle list/unlist of tokens
    // because we don't have the Resolver Set event on some networks
    // We can remove this once we have migrated data to a new resolver which emits this event on
    // all networks.
    token = handleTokenRPCCalls(token, resolverAddress);

    token.save();

    return token as Token;
}

export function getOrInitTokenGovernanceConfig(
    block: ethereum.Block,
    superTokenAddress: Address
): TokenGovernanceConfig {
    if (superTokenAddress.equals(ZERO_ADDRESS)) {
        let governanceConfig = TokenGovernanceConfig.load(
            ZERO_ADDRESS.toHexString()
        );
        if (governanceConfig == null) {
            governanceConfig = new TokenGovernanceConfig(
                ZERO_ADDRESS.toHexString()
            );
            governanceConfig.createdAtTimestamp = block.timestamp;
            governanceConfig.createdAtBlockNumber = block.number;
            governanceConfig.updatedAtBlockNumber = block.number;
            governanceConfig.updatedAtTimestamp = block.timestamp;
            governanceConfig.isDefault = true;
            governanceConfig.rewardAddress = ZERO_ADDRESS;
            governanceConfig.liquidationPeriod = BIG_INT_ZERO;
            governanceConfig.patricianPeriod = BIG_INT_ZERO;
            governanceConfig.minimumDeposit = BIG_INT_ZERO;

            governanceConfig.save();
        }
        return governanceConfig;
    } else {
        let governanceConfig = TokenGovernanceConfig.load(
            superTokenAddress.toHexString()
        );
        if (governanceConfig == null) {
            governanceConfig = new TokenGovernanceConfig(
                superTokenAddress.toHexString()
            );
            governanceConfig.createdAtTimestamp = block.timestamp;
            governanceConfig.createdAtBlockNumber = block.number;
            governanceConfig.updatedAtBlockNumber = block.number;
            governanceConfig.updatedAtTimestamp = block.timestamp;
            governanceConfig.isDefault = false;
            governanceConfig.rewardAddress = null;
            governanceConfig.liquidationPeriod = null;
            governanceConfig.patricianPeriod = null;
            governanceConfig.minimumDeposit = null;
            governanceConfig.token = superTokenAddress.toHexString();

            governanceConfig.save();
        }
        return governanceConfig;
    }
}

/**
 * Create a token entity for regular ERC20 tokens.
 * These are the underlying tokens for
 */
export function getOrInitToken(
    tokenAddress: Address,
    block: ethereum.Block,
    resolverAddress: Address
): void {
    const tokenId = tokenAddress.toHex();
    let token = new Token(tokenId);

    if (tokenAddress.equals(ZERO_ADDRESS)) {
        return;
    }

    token.createdAtTimestamp = block.timestamp;
    token.decimals = 0;
    token.name = "";
    token.symbol = "";
    token.createdAtBlockNumber = block.number;
    token.isSuperToken = false;
    token.isNativeAssetSuperToken = false;
    token.isListed = false;

    token = handleTokenRPCCalls(token, resolverAddress);
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
    const streamRevisionId = getStreamRevisionID(
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
 * NOTE: this is only called in one place in handleFlowUpdated
 * and we always save the Stream entity OUTSIDE of this function
 * after initializing it here.
 */
export function getOrInitStream(event: FlowUpdated): Stream {
    // Create accounts if they do not exist
    getOrInitAccount(event.params.sender, event.block);
    getOrInitAccount(event.params.receiver, event.block);

    // Create a streamRevision entity for this stream if one doesn't exist.
    const streamRevision = getOrInitStreamRevision(
        event.params.sender,
        event.params.receiver,
        event.params.token
    );
    const id = getStreamID(
        event.params.sender,
        event.params.receiver,
        event.params.token,
        streamRevision.revisionIndex
    );

    // set stream id
    streamRevision.mostRecentStream = id;
    streamRevision.save();

    let stream = Stream.load(id);
    if (stream == null) {
        const currentTimestamp = event.block.timestamp;
        stream = new Stream(id);
        stream.createdAtTimestamp = currentTimestamp;
        stream.createdAtBlockNumber = event.block.number;
        stream.token = event.params.token.toHex();
        stream.sender = event.params.sender.toHex();
        stream.receiver = event.params.receiver.toHex();
        stream.currentFlowRate = BigInt.fromI32(0);
        stream.deposit = BigInt.fromI32(0);
        stream.streamedUntilUpdatedAt = BigInt.fromI32(0);
        stream.updatedAtTimestamp = currentTimestamp;
        stream.updatedAtBlockNumber = event.block.number;
        stream.userData = event.params.userData;

        // Check if token exists and create here if not.
        // handles chain "native" tokens (e.g. ETH, MATIC, xDAI)
        // also handles the fact that custom super tokens are
        // initialized after event is first initialized
        getOrInitSuperToken(event, event.params.token, "FlowUpdated");
    }
    return stream as Stream;
}

export function getOrInitFlowOperator(
    block: ethereum.Block,
    flowOperatorAddress: Address,
    tokenAddress: Address,
    senderAddress: Address
): FlowOperator {
    const flowOperatorId = getFlowOperatorID(
        flowOperatorAddress,
        tokenAddress,
        senderAddress
    );
    const currentTimestamp = block.timestamp;
    let flowOperatorEntity = FlowOperator.load(flowOperatorId);
    if (flowOperatorEntity == null) {
        flowOperatorEntity = new FlowOperator(flowOperatorId);
        flowOperatorEntity.createdAtBlockNumber = block.number;
        flowOperatorEntity.createdAtTimestamp = currentTimestamp;
        flowOperatorEntity.permissions = 0;
        flowOperatorEntity.flowRateAllowanceGranted = BigInt.fromI32(0);
        flowOperatorEntity.allowance = BigInt.fromI32(0);
        flowOperatorEntity.flowRateAllowanceRemaining = BigInt.fromI32(0);
        flowOperatorEntity.token = tokenAddress.toHex();

        // https://github.com/superfluid-finance/protocol-monorepo/issues/1397
        const sender = getOrInitAccount(senderAddress, block);
        flowOperatorEntity.sender = sender.id;

        // https://github.com/superfluid-finance/protocol-monorepo/issues/1397
        const accountTokenSnapshot = getOrInitAccountTokenSnapshot(
            senderAddress,
            tokenAddress,
            block
        );
        flowOperatorEntity.accountTokenSnapshot = accountTokenSnapshot.id;
        flowOperatorEntity.flowOperator = flowOperatorAddress;
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
    event: ethereum.Event,
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    indexCreatedId: string,
    triggeredByEventName: string
): Index {
    const indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    const block = event.block;
    const currentTimestamp = block.timestamp;
    let index = Index.load(indexEntityId);
    if (index == null) {
        const publisherId = publisherAddress.toHex();
        const tokenId = tokenAddress.toHex();
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
        getOrInitSuperToken(event, tokenAddress, triggeredByEventName);
    }
    index.updatedAtTimestamp = currentTimestamp;
    index.updatedAtBlockNumber = block.number;
    return index as Index;
}

/**
 * Gets or initializes a Subscription, always sets the updatedAt.
 */
export function getOrInitSubscription(
    event: ethereum.Event,
    subscriberAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
    indexId: BigInt,
    triggeredByEventName: string
): IndexSubscription {
    const subscriptionId = getSubscriptionID(
        subscriberAddress,
        publisherAddress,
        tokenAddress,
        indexId
    );
    let subscription = IndexSubscription.load(subscriptionId);
    const indexEntityId = getIndexID(publisherAddress, tokenAddress, indexId);
    const block = event.block;
    const currentTimestamp = block.timestamp;

    if (subscription == null) {
        const index = getOrInitIndex(
            event,
            publisherAddress,
            tokenAddress,
            indexId,
            "",
            triggeredByEventName
        );

        subscription = new IndexSubscription(subscriptionId);
        subscription.createdAtTimestamp = currentTimestamp;
        subscription.createdAtBlockNumber = block.number;
        subscription.subscriber = subscriberAddress.toHex();
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

export function getOrInitResolverEntry(
    id: string,
    target: Address,
    block: ethereum.Block
): ResolverEntry {
    let resolverEntry = ResolverEntry.load(id);

    if (resolverEntry == null) {
        resolverEntry = new ResolverEntry(id);
        resolverEntry.createdAtBlockNumber = block.number;
        resolverEntry.createdAtTimestamp = block.timestamp;
        resolverEntry.targetAddress = target;

        const superToken = Token.load(target.toHex());
        resolverEntry.isToken = superToken != null;
    }
    resolverEntry.updatedAtBlockNumber = block.number;
    resolverEntry.updatedAtTimestamp = block.timestamp;
    resolverEntry.isListed = target.notEqual(ZERO_ADDRESS);

    resolverEntry.save();
    return resolverEntry as ResolverEntry;
}

/**************************************************************************
 * Aggregate initializer functions
 *************************************************************************/
export function getOrInitAccountTokenSnapshot(
    accountAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block
): AccountTokenSnapshot {
    const atsId = getAccountTokenSnapshotID(accountAddress, tokenAddress);
    let accountTokenSnapshot = AccountTokenSnapshot.load(atsId);

if (accountTokenSnapshot == null) {
        accountTokenSnapshot = new AccountTokenSnapshot(atsId);
        accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
        accountTokenSnapshot.updatedAtBlockNumber = block.number;
        accountTokenSnapshot.totalNumberOfActiveStreams = 0;
        accountTokenSnapshot.activeIncomingStreamCount = 0;
        accountTokenSnapshot.activeOutgoingStreamCount = 0;
        accountTokenSnapshot.inactiveIncomingStreamCount = 0;
        accountTokenSnapshot.inactiveOutgoingStreamCount = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalSubscriptionsWithUnits = 0;
        accountTokenSnapshot.isLiquidationEstimateOptimistic = false;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.balanceUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalInflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalDeposit = BIG_INT_ZERO;
        accountTokenSnapshot.maybeCriticalAtTimestamp = null;
        accountTokenSnapshot.account = accountAddress.toHex();
        accountTokenSnapshot.token = tokenAddress.toHex();
        accountTokenSnapshot.save();

        const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
        tokenStatistic.totalNumberOfAccounts = tokenStatistic.totalNumberOfAccounts + 1;
        tokenStatistic.save();

    }
    
    return accountTokenSnapshot as AccountTokenSnapshot;
}

export function _createAccountTokenSnapshotLogEntity(
    event: ethereum.Event,
    accountAddress: Address,
    tokenAddress: Address,
    eventName: string
): void {
    if (accountAddress.equals(ZERO_ADDRESS)) {
        return;
    }
    const accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
        event.block
    );
    // Transaction
    const atsLog = new AccountTokenSnapshotLog(
        createLogID("ATSLog", accountTokenSnapshot.id, event)
    );
    atsLog.transactionHash = event.transaction.hash;
    atsLog.timestamp = event.block.timestamp;
    atsLog.order = getOrder(event.block.number, event.logIndex);
    atsLog.blockNumber = event.block.number;
    atsLog.logIndex = event.logIndex;
    atsLog.triggeredByEventName = eventName;
    // Account token snapshot state
    atsLog.totalNumberOfActiveStreams =
        accountTokenSnapshot.totalNumberOfActiveStreams;
    atsLog.activeIncomingStreamCount =
        accountTokenSnapshot.activeIncomingStreamCount;
    atsLog.activeOutgoingStreamCount =
        accountTokenSnapshot.activeOutgoingStreamCount;
    atsLog.totalNumberOfClosedStreams =
        accountTokenSnapshot.totalNumberOfClosedStreams;
    atsLog.inactiveIncomingStreamCount =
        accountTokenSnapshot.inactiveIncomingStreamCount;
    atsLog.inactiveOutgoingStreamCount =
        accountTokenSnapshot.inactiveOutgoingStreamCount;
    atsLog.totalSubscriptionsWithUnits =
        accountTokenSnapshot.totalSubscriptionsWithUnits;
    atsLog.totalApprovedSubscriptions =
        accountTokenSnapshot.totalApprovedSubscriptions;
    atsLog.balance = accountTokenSnapshot.balanceUntilUpdatedAt;
    atsLog.totalNetFlowRate = accountTokenSnapshot.totalNetFlowRate;
    atsLog.totalInflowRate = accountTokenSnapshot.totalInflowRate;
    atsLog.totalOutflowRate = accountTokenSnapshot.totalOutflowRate;
    atsLog.totalAmountStreamed =
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt;
    atsLog.totalAmountStreamedIn =
        accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt;
    atsLog.totalAmountStreamedOut =
        accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt;
    atsLog.totalAmountTransferred =
        accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt;
    atsLog.totalDeposit = accountTokenSnapshot.totalDeposit;
    atsLog.maybeCriticalAtTimestamp =
        accountTokenSnapshot.maybeCriticalAtTimestamp;
    atsLog.account = accountTokenSnapshot.account;
    atsLog.token = accountTokenSnapshot.token;
    atsLog.accountTokenSnapshot = accountTokenSnapshot.id;
    atsLog.save();
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
        tokenStatistic.totalNumberOfAccounts = 0;
        tokenStatistic.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountTransferredUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalSupply = BIG_INT_ZERO;
        tokenStatistic.totalDeposit = BIG_INT_ZERO;
        tokenStatistic.totalNumberOfHolders = 0;
        tokenStatistic.token = tokenId;
        tokenStatistic.save();
    }
    return tokenStatistic as TokenStatistic;
}

export function _createTokenStatisticLogEntity(
    event: ethereum.Event,
    tokenAddress: Address,
    eventName: string
): void {
    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, event.block);
    const tokenStatisticLog = new TokenStatisticLog(
        createLogID("TSLog", tokenStatistic.id, event)
    );

    // Transaction Data
    tokenStatisticLog.timestamp = event.block.timestamp;
    tokenStatisticLog.blockNumber = event.block.number;
    tokenStatisticLog.transactionHash = event.transaction.hash;
    tokenStatisticLog.logIndex = event.logIndex;
    tokenStatisticLog.order = getOrder(event.block.number, event.logIndex);
    tokenStatisticLog.triggeredByEventName = eventName;

    // Token Statistic State
    tokenStatisticLog.totalNumberOfActiveStreams =
        tokenStatistic.totalNumberOfActiveStreams;
    tokenStatisticLog.totalNumberOfClosedStreams =
        tokenStatistic.totalNumberOfClosedStreams;
    tokenStatisticLog.totalNumberOfIndexes =
        tokenStatistic.totalNumberOfIndexes;
    tokenStatisticLog.totalNumberOfActiveIndexes =
        tokenStatistic.totalNumberOfActiveIndexes;
    tokenStatisticLog.totalSubscriptionsWithUnits =
        tokenStatistic.totalSubscriptionsWithUnits;
    tokenStatisticLog.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions;
    tokenStatisticLog.totalDeposit = tokenStatistic.totalDeposit;
    tokenStatisticLog.totalOutflowRate = tokenStatistic.totalOutflowRate;
    tokenStatisticLog.totalAmountStreamed =
        tokenStatistic.totalAmountStreamedUntilUpdatedAt;
    tokenStatisticLog.totalAmountTransferred =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt;
    tokenStatisticLog.totalAmountDistributed =
        tokenStatistic.totalAmountDistributedUntilUpdatedAt;
    tokenStatisticLog.totalSupply = tokenStatistic.totalSupply;
    tokenStatisticLog.token = tokenStatistic.token;
    tokenStatisticLog.totalNumberOfAccounts = tokenStatistic.totalNumberOfAccounts;
    tokenStatisticLog.totalNumberOfHolders  = tokenStatistic.totalNumberOfHolders;
    tokenStatisticLog.tokenStatistic = tokenStatistic.id;
    tokenStatisticLog.save();
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
    const account = getOrInitAccount(accountAddress, block);
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
    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    const totalSubscriptionWithUnitsDelta =
        isDeletingSubscription && subscriptionWithUnitsExists
            ? -1
            : isIncrementingSubWithUnits && !subscriptionWithUnitsExists
            ? 1
            : 0;
    const totalApprovedSubscriptionsDelta = isApproving
        ? 1
        : isRevokingSubscription && subscriptionApproved
        ? -1
        : 0;

    // update ATS Subscription data
    const accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
        block
    );

    accountTokenSnapshot.totalSubscriptionsWithUnits =
        accountTokenSnapshot.totalSubscriptionsWithUnits +
        totalSubscriptionWithUnitsDelta;
    accountTokenSnapshot.isLiquidationEstimateOptimistic =
        accountTokenSnapshot.totalSubscriptionsWithUnits > 0;
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
    accountTokenSnapshot.isLiquidationEstimateOptimistic =
        accountTokenSnapshot.totalSubscriptionsWithUnits > 0;
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
    block: ethereum.Block,
    balanceDelta: BigInt | null
): AccountTokenSnapshot {
    const superTokenContract = SuperToken.bind(
        Address.fromString(accountTokenSnapshot.token)
    );

    if (balanceDelta && accountTokenSnapshot.totalSubscriptionsWithUnits == 0) {
        accountTokenSnapshot.balanceUntilUpdatedAt =
            accountTokenSnapshot.balanceUntilUpdatedAt.plus(
                balanceDelta as BigInt
            );
    } else {
        // if the account has any subscriptions with units we assume that
        // the balance data requires a RPC call for balance because we did not
        // have claim events there and we do not count distributions
        // for subscribers
        const newBalanceResult = superTokenContract.try_realtimeBalanceOf(
            Address.fromString(accountTokenSnapshot.account),
            block.timestamp
        );
        if (!newBalanceResult.reverted) {
            accountTokenSnapshot.balanceUntilUpdatedAt =
                newBalanceResult.value.value0;
        }
    }

    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlockNumber = block.number;

    accountTokenSnapshot.maybeCriticalAtTimestamp =
        calculateMaybeCriticalAtTimestamp(
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.balanceUntilUpdatedAt,
            accountTokenSnapshot.totalNetFlowRate,
            accountTokenSnapshot.maybeCriticalAtTimestamp
        );

    accountTokenSnapshot.save();
    return accountTokenSnapshot as AccountTokenSnapshot;
}

/**
 * Updates the amount streamed, balance until updated at for the AccountTokenSnapshot
 * entity and also updates the updatedAt property on the account entity.
 * NOTE: call before the `updatedAt` property is updated otherwise you get incorrect data;
 * NOTE: you should be calling updateTokenStatsStreamedUntilUpdatedAt whenever you call this
 */
export function updateATSStreamedAndBalanceUntilUpdatedAt(
    accountAddress: Address,
    tokenAddress: Address,
    block: ethereum.Block,

    // TODO: we are currently always passing null here
    // remove null one at a time and use validation script
    // to compare v1 to feature
    balanceDelta: BigInt | null
): void {
    let accountTokenSnapshot = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
        block
    );

    const balanceUntilUpdatedAtBeforeUpdate = accountTokenSnapshot.balanceUntilUpdatedAt;
    
    const amountStreamedSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalNetFlowRate
        );
    const amountStreamedInSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalInflowRate
        );
    const amountStreamedOutSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalOutflowRate
        );

    // update the totalStreamedUntilUpdatedAt (net)
    accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdatedAt
        );

    // update the totalStreamedUntilUpdatedAt (in)
    accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt.plus(
            amountStreamedInSinceLastUpdatedAt
        );

    // update the totalStreamedUntilUpdatedAt (out)
    accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt.plus(
            amountStreamedOutSinceLastUpdatedAt
        );

    const netAmountStreamedInSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalNetFlowRate
        );

    // update the balance via external call if account has any subscription with more than 0 units
    // or uses the balance delta (which includes amount streamed) and saves the entity
    // we always add the amount streamed in this function
    accountTokenSnapshot = updateATSBalanceAndUpdatedAt(
        accountTokenSnapshot,
        block,
        balanceDelta
            ? balanceDelta.plus(netAmountStreamedInSinceLastUpdatedAt)
            : balanceDelta
    );
    accountTokenSnapshot.save();

    const balanceUntilUpdatedAtAfterUpdate = accountTokenSnapshot.balanceUntilUpdatedAt;

    if (
        balanceUntilUpdatedAtBeforeUpdate.equals(BIG_INT_ZERO) &&
        balanceUntilUpdatedAtAfterUpdate.gt(BIG_INT_ZERO)
    ) {
        const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
        tokenStatistic.totalNumberOfHolders =
            tokenStatistic.totalNumberOfHolders + 1;
        tokenStatistic.save();
    } else if (
        balanceUntilUpdatedAtAfterUpdate.equals(BIG_INT_ZERO) &&
        balanceUntilUpdatedAtBeforeUpdate.gt(BIG_INT_ZERO)
    ) {
        const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
        tokenStatistic.totalNumberOfHolders =
            tokenStatistic.totalNumberOfHolders - 1;
        // TODO: this should be not possible, since we first receive money and then spend it.
        if (tokenStatistic.totalNumberOfHolders < 0) {
            tokenStatistic.totalNumberOfHolders = 0;
        }
        tokenStatistic.save();
    }


    // update the updatedAt property of the account that just made an update
    updateAccountUpdatedAt(accountAddress, block);
}

/**
 * This function should always be called with updateATSStreamedAndBalanceUntilUpdatedAt
 * @param tokenAddress
 * @param block
 */
export function updateTokenStatsStreamedUntilUpdatedAt(
    tokenAddress: Address,
    block: ethereum.Block
): void {
    const tokenStats = getOrInitTokenStatistic(tokenAddress, block);
    const amountStreamedSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            tokenStats.updatedAtTimestamp,
            tokenStats.totalOutflowRate
        );
    tokenStats.totalAmountStreamedUntilUpdatedAt =
        tokenStats.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdatedAt
        );
    tokenStats.updatedAtTimestamp = block.timestamp;
    tokenStats.updatedAtBlockNumber = block.number;
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
    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    const totalNumberOfActiveStreamsDelta = isCreate ? 1 : isDelete ? -1 : 0;
    const totalNumberOfClosedStreamsDelta = isDelete ? 1 : 0;
    const tokenStatsAmountStreamedSinceLastUpdate =
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

    const senderATS = getOrInitAccountTokenSnapshot(
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
    senderATS.activeOutgoingStreamCount =
        senderATS.activeOutgoingStreamCount + totalNumberOfActiveStreamsDelta;
    senderATS.inactiveOutgoingStreamCount =
        senderATS.inactiveOutgoingStreamCount + totalNumberOfClosedStreamsDelta;

    senderATS.totalNumberOfClosedStreams =
        senderATS.totalNumberOfClosedStreams + totalNumberOfClosedStreamsDelta;
    senderATS.totalDeposit = senderATS.totalDeposit.plus(depositDelta);
    senderATS.maybeCriticalAtTimestamp = calculateMaybeCriticalAtTimestamp(
        senderATS.updatedAtTimestamp,
        senderATS.balanceUntilUpdatedAt,
        senderATS.totalNetFlowRate,
        senderATS.maybeCriticalAtTimestamp
    );

    const receiverATS = getOrInitAccountTokenSnapshot(
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
    receiverATS.activeIncomingStreamCount =
        receiverATS.activeIncomingStreamCount + totalNumberOfActiveStreamsDelta;
    receiverATS.inactiveIncomingStreamCount =
        receiverATS.inactiveIncomingStreamCount +
        totalNumberOfClosedStreamsDelta;

    receiverATS.totalNumberOfClosedStreams =
        receiverATS.totalNumberOfClosedStreams +
        totalNumberOfClosedStreamsDelta;

    receiverATS.maybeCriticalAtTimestamp = calculateMaybeCriticalAtTimestamp(
        receiverATS.updatedAtTimestamp,
        receiverATS.balanceUntilUpdatedAt,
        receiverATS.totalNetFlowRate,
        receiverATS.maybeCriticalAtTimestamp
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
    const fromAccountTokenSnapshot = getOrInitAccountTokenSnapshot(
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

    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    tokenStatistic.totalAmountTransferredUntilUpdatedAt =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt.plus(value);
    tokenStatistic.save();
}
