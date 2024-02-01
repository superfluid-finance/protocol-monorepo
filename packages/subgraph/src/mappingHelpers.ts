import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { ISuperfluid as Superfluid } from "../generated/Host/ISuperfluid";
import {
    Account,
    AccountTokenSnapshot,
    AccountTokenSnapshotLog,
    FlowOperator,
    Index,
    IndexSubscription,
    Pool,
    PoolDistributor,
    PoolMember,
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
    getPoolMemberID,
    getPoolDistributorID,
    getActiveStreamsDelta,
    getClosedStreamsDelta,
    MAX_UINT256,
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

export function getOrInitPool(event: ethereum.Event, poolId: string): Pool {
    // get existing pool
    let pool = Pool.load(poolId);

    // init new pool if non-existent
    if (pool == null) {
        pool = new Pool(poolId);
        pool.createdAtTimestamp = event.block.timestamp;
        pool.createdAtBlockNumber = event.block.number;
        pool.updatedAtTimestamp = event.block.timestamp;
        pool.updatedAtBlockNumber = event.block.number;

        pool.totalUnits = BIG_INT_ZERO;
        pool.totalConnectedUnits = BIG_INT_ZERO;
        pool.totalDisconnectedUnits = BIG_INT_ZERO;
        pool.totalAmountInstantlyDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalAmountFlowedDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        pool.totalMembers = 0;
        pool.totalConnectedMembers = 0;
        pool.totalDisconnectedMembers = 0;
        pool.adjustmentFlowRate = BIG_INT_ZERO;
        pool.flowRate = BIG_INT_ZERO;
        pool.totalBuffer = BIG_INT_ZERO;
        pool.token = ZERO_ADDRESS.toHex();
        pool.admin = ZERO_ADDRESS.toHex();
    }

    return pool;
}

export function updatePoolTotalAmountFlowedAndDistributed(
    event: ethereum.Event,
    pool: Pool
): Pool {
    const timeDelta = event.block.timestamp.minus(pool.updatedAtTimestamp);
    const amountFlowedSinceLastUpdate = pool.flowRate.times(timeDelta);

    pool.updatedAtBlockNumber = event.block.number;
    pool.updatedAtTimestamp = event.block.timestamp;

    pool.totalAmountFlowedDistributedUntilUpdatedAt =
        pool.totalAmountFlowedDistributedUntilUpdatedAt.plus(
            amountFlowedSinceLastUpdate
        );
    pool.totalAmountDistributedUntilUpdatedAt =
        pool.totalAmountDistributedUntilUpdatedAt.plus(
            amountFlowedSinceLastUpdate
        );

    pool.save();

    return pool;
}

export function getOrInitPoolMember(
    event: ethereum.Event,
    poolAddress: Address,
    poolMemberAddress: Address
): PoolMember {
    const poolMemberID = getPoolMemberID(poolAddress, poolMemberAddress);
    let poolMember = PoolMember.load(poolMemberID);

    if (poolMember == null) {
        poolMember = new PoolMember(poolMemberID);
        poolMember.createdAtTimestamp = event.block.timestamp;
        poolMember.createdAtBlockNumber = event.block.number;
        poolMember.updatedAtTimestamp = event.block.timestamp;
        poolMember.updatedAtBlockNumber = event.block.number;

        poolMember.units = BIG_INT_ZERO;
        poolMember.isConnected = false;
        poolMember.totalAmountClaimed = BIG_INT_ZERO;
        poolMember.poolTotalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        poolMember.totalAmountReceivedUntilUpdatedAt = BIG_INT_ZERO;

        poolMember.account = poolMemberAddress.toHex();
        poolMember.pool = poolAddress.toHex();
    }

    return poolMember;
}

export function getOrInitPoolDistributor(
    event: ethereum.Event,
    poolAddress: Address,
    poolDistributorAddress: Address
): PoolDistributor {
    const poolDistributorID = getPoolDistributorID(
        poolAddress,
        poolDistributorAddress
    );
    let poolDistributor = PoolDistributor.load(poolDistributorID);

    if (poolDistributor == null) {
        poolDistributor = new PoolDistributor(poolDistributorID);
        poolDistributor.createdAtTimestamp = event.block.timestamp;
        poolDistributor.createdAtBlockNumber = event.block.number;
        poolDistributor.updatedAtTimestamp = event.block.timestamp;
        poolDistributor.updatedAtBlockNumber = event.block.number;

        poolDistributor.totalAmountInstantlyDistributedUntilUpdatedAt =
            BIG_INT_ZERO;
        poolDistributor.totalAmountFlowedDistributedUntilUpdatedAt =
            BIG_INT_ZERO;
        poolDistributor.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        poolDistributor.totalBuffer = BIG_INT_ZERO;
        poolDistributor.flowRate = BIG_INT_ZERO;

        poolDistributor.account = poolDistributorAddress.toHex();
        poolDistributor.pool = poolAddress.toHex();
    }

    return poolDistributor;
}
export function updatePoolDistributorTotalAmountFlowedAndDistributed(
    event: ethereum.Event,
    poolDistributor: PoolDistributor
): PoolDistributor {
    const timeDelta = event.block.timestamp.minus(
        poolDistributor.updatedAtTimestamp
    );
    const amountFlowedSinceLastUpdate =
        poolDistributor.flowRate.times(timeDelta);

    poolDistributor.updatedAtBlockNumber = event.block.number;
    poolDistributor.updatedAtTimestamp = event.block.timestamp;

    poolDistributor.totalAmountFlowedDistributedUntilUpdatedAt =
        poolDistributor.totalAmountFlowedDistributedUntilUpdatedAt.plus(
            amountFlowedSinceLastUpdate
        );
    poolDistributor.totalAmountDistributedUntilUpdatedAt =
        poolDistributor.totalAmountDistributedUntilUpdatedAt.plus(
            amountFlowedSinceLastUpdate
        );

    poolDistributor.save();

    return poolDistributor;
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
        accountTokenSnapshot.totalCFANumberOfActiveStreams = 0;
        accountTokenSnapshot.totalGDANumberOfActiveStreams = 0;
        accountTokenSnapshot.activeIncomingStreamCount = 0;
        accountTokenSnapshot.activeOutgoingStreamCount = 0;
        accountTokenSnapshot.activeCFAOutgoingStreamCount = 0;
        accountTokenSnapshot.activeGDAOutgoingStreamCount = 0;
        accountTokenSnapshot.inactiveIncomingStreamCount = 0;
        accountTokenSnapshot.inactiveOutgoingStreamCount = 0;
        accountTokenSnapshot.inactiveCFAOutgoingStreamCount = 0;
        accountTokenSnapshot.inactiveGDAOutgoingStreamCount = 0;
        accountTokenSnapshot.totalNumberOfClosedStreams = 0;
        accountTokenSnapshot.totalCFANumberOfClosedStreams = 0;
        accountTokenSnapshot.totalGDANumberOfClosedStreams = 0;
        accountTokenSnapshot.isLiquidationEstimateOptimistic = false;
        accountTokenSnapshot.totalSubscriptionsWithUnits = 0;
        accountTokenSnapshot.totalApprovedSubscriptions = 0;
        accountTokenSnapshot.totalMembershipsWithUnits = 0;
        accountTokenSnapshot.totalConnectedMemberships = 0;
        accountTokenSnapshot.balanceUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalNetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalCFANetFlowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalInflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalCFAOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalGDAOutflowRate = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalCFAAmountStreamedOutUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        accountTokenSnapshot.totalCFAAmountStreamedUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalAmountTransferredUntilUpdatedAt =
            BIG_INT_ZERO;
        accountTokenSnapshot.totalDeposit = BIG_INT_ZERO;
        accountTokenSnapshot.totalCFADeposit = BIG_INT_ZERO;
        accountTokenSnapshot.totalGDADeposit = BIG_INT_ZERO;
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
    const ats = getOrInitAccountTokenSnapshot(
        accountAddress,
        tokenAddress,
        event.block
    );
    // Transaction
    const atsLog = new AccountTokenSnapshotLog(
        createLogID("ATSLog", ats.id, event)
    );
    atsLog.transactionHash = event.transaction.hash;
    atsLog.timestamp = event.block.timestamp;
    atsLog.order = getOrder(event.block.number, event.logIndex);
    atsLog.blockNumber = event.block.number;
    atsLog.logIndex = event.logIndex;
    atsLog.triggeredByEventName = eventName;
    // Account token snapshot state
    atsLog.totalNumberOfActiveStreams = ats.totalNumberOfActiveStreams;
    atsLog.totalCFANumberOfActiveStreams = ats.totalCFANumberOfActiveStreams;
    atsLog.totalGDANumberOfActiveStreams = ats.totalGDANumberOfActiveStreams;
    atsLog.activeIncomingStreamCount = ats.activeIncomingStreamCount;
    atsLog.activeOutgoingStreamCount = ats.activeOutgoingStreamCount;
    atsLog.activeCFAOutgoingStreamCount = ats.activeCFAOutgoingStreamCount;
    atsLog.activeGDAOutgoingStreamCount = ats.activeGDAOutgoingStreamCount;
    atsLog.totalNumberOfClosedStreams = ats.totalNumberOfClosedStreams;
    atsLog.totalCFANumberOfClosedStreams = ats.totalCFANumberOfClosedStreams;
    atsLog.totalGDANumberOfClosedStreams = ats.totalGDANumberOfClosedStreams;
    atsLog.inactiveIncomingStreamCount = ats.inactiveIncomingStreamCount;
    atsLog.inactiveOutgoingStreamCount = ats.inactiveOutgoingStreamCount;
    atsLog.inactiveCFAOutgoingStreamCount = ats.inactiveCFAOutgoingStreamCount;
    atsLog.inactiveGDAOutgoingStreamCount = ats.inactiveGDAOutgoingStreamCount;
    atsLog.totalSubscriptionsWithUnits = ats.totalSubscriptionsWithUnits;
    atsLog.totalApprovedSubscriptions = ats.totalApprovedSubscriptions;
    atsLog.totalMembershipsWithUnits = ats.totalMembershipsWithUnits;
    atsLog.totalConnectedMemberships = ats.totalConnectedMemberships;
    atsLog.balance = ats.balanceUntilUpdatedAt;
    atsLog.totalNetFlowRate = ats.totalNetFlowRate;
    atsLog.totalCFANetFlowRate = ats.totalCFANetFlowRate;
    atsLog.totalInflowRate = ats.totalInflowRate;
    atsLog.totalOutflowRate = ats.totalOutflowRate;
    atsLog.totalCFAOutflowRate = ats.totalCFAOutflowRate;
    atsLog.totalGDAOutflowRate = ats.totalGDAOutflowRate;
    atsLog.totalAmountStreamed = ats.totalAmountStreamedUntilUpdatedAt;
    atsLog.totalCFAAmountStreamed = ats.totalCFAAmountStreamedUntilUpdatedAt;
    atsLog.totalAmountStreamedIn = ats.totalAmountStreamedInUntilUpdatedAt;
    atsLog.totalAmountStreamedOut = ats.totalAmountStreamedOutUntilUpdatedAt;
    atsLog.totalCFAAmountStreamedOut =
        ats.totalCFAAmountStreamedOutUntilUpdatedAt;
    atsLog.totalAmountTransferred = ats.totalAmountTransferredUntilUpdatedAt;
    atsLog.totalDeposit = ats.totalDeposit;
    atsLog.totalCFADeposit = ats.totalCFADeposit;
    atsLog.totalGDADeposit = ats.totalGDADeposit;
    atsLog.maybeCriticalAtTimestamp = ats.maybeCriticalAtTimestamp;
    atsLog.account = ats.account;
    atsLog.token = ats.token;
    atsLog.accountTokenSnapshot = ats.id;
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
        tokenStatistic.totalCFANumberOfActiveStreams = 0;
        tokenStatistic.totalGDANumberOfActiveStreams = 0;
        tokenStatistic.totalNumberOfClosedStreams = 0;
        tokenStatistic.totalCFANumberOfClosedStreams = 0;
        tokenStatistic.totalGDANumberOfClosedStreams = 0;
        tokenStatistic.totalNumberOfIndexes = 0;
        tokenStatistic.totalNumberOfActiveIndexes = 0;
        tokenStatistic.totalSubscriptionsWithUnits = 0;
        tokenStatistic.totalApprovedSubscriptions = 0;
        tokenStatistic.totalNumberOfPools = 0;
        tokenStatistic.totalNumberOfActivePools = 0;
        tokenStatistic.totalMembershipsWithUnits = 0;
        tokenStatistic.totalConnectedMemberships = 0;
        tokenStatistic.totalOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalCFAOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalGDAOutflowRate = BIG_INT_ZERO;
        tokenStatistic.totalNumberOfAccounts = 0;
        tokenStatistic.totalAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalCFAAmountStreamedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountTransferredUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalAmountDistributedUntilUpdatedAt = BIG_INT_ZERO;
        tokenStatistic.totalSupply = BIG_INT_ZERO;
        tokenStatistic.totalDeposit = BIG_INT_ZERO;
        tokenStatistic.totalCFADeposit = BIG_INT_ZERO;
        tokenStatistic.totalGDADeposit = BIG_INT_ZERO;
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
    tokenStatisticLog.totalCFANumberOfActiveStreams =
        tokenStatistic.totalCFANumberOfActiveStreams;
    tokenStatisticLog.totalGDANumberOfActiveStreams =
        tokenStatistic.totalGDANumberOfActiveStreams;
    tokenStatisticLog.totalNumberOfClosedStreams =
        tokenStatistic.totalNumberOfClosedStreams;
    tokenStatisticLog.totalCFANumberOfClosedStreams =
        tokenStatistic.totalCFANumberOfClosedStreams;
    tokenStatisticLog.totalGDANumberOfClosedStreams =
        tokenStatistic.totalGDANumberOfClosedStreams;
    tokenStatisticLog.totalNumberOfIndexes =
        tokenStatistic.totalNumberOfIndexes;
    tokenStatisticLog.totalNumberOfActiveIndexes =
        tokenStatistic.totalNumberOfActiveIndexes;
    tokenStatisticLog.totalSubscriptionsWithUnits =
        tokenStatistic.totalSubscriptionsWithUnits;
    tokenStatisticLog.totalApprovedSubscriptions =
        tokenStatistic.totalApprovedSubscriptions;
    tokenStatisticLog.totalNumberOfPools = tokenStatistic.totalNumberOfPools;
    tokenStatisticLog.totalNumberOfActivePools =
        tokenStatistic.totalNumberOfActivePools;
    tokenStatisticLog.totalMembershipsWithUnits =
        tokenStatistic.totalMembershipsWithUnits;
    tokenStatisticLog.totalConnectedMemberships =
        tokenStatistic.totalConnectedMemberships;
    tokenStatisticLog.totalDeposit = tokenStatistic.totalDeposit;
    tokenStatisticLog.totalCFADeposit = tokenStatistic.totalCFADeposit;
    tokenStatisticLog.totalGDADeposit = tokenStatistic.totalGDADeposit;
    tokenStatisticLog.totalOutflowRate = tokenStatistic.totalOutflowRate;
    tokenStatisticLog.totalCFAOutflowRate = tokenStatistic.totalCFAOutflowRate;
    tokenStatisticLog.totalGDAOutflowRate = tokenStatistic.totalGDAOutflowRate;
    tokenStatisticLog.totalAmountStreamed =
        tokenStatistic.totalAmountStreamedUntilUpdatedAt;
    tokenStatisticLog.totalCFAAmountStreamed =
        tokenStatistic.totalCFAAmountStreamedUntilUpdatedAt;
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
 * Updates ATS and TokenStats distribution agreement data (IDA or GDA).
 */
export function updateAggregateDistributionAgreementData(
    accountAddress: Address,
    tokenAddress: Address,
    subscriptionWithUnitsExists: boolean,
    subscriptionApproved: boolean,
    isIncrementingSubWithUnits: boolean,
    isRevokingSubscription: boolean,
    isDeletingSubscription: boolean,
    isApproving: boolean,
    block: ethereum.Block,
    isIDA: boolean
): void {
    const totalSubscriptionWithUnitsDelta =
        // we only decrement if the subscription exists and we are deleting
        isDeletingSubscription && subscriptionWithUnitsExists
            ? -1
            : // we only increment if the subscription does not exist and we are incrementing
            isIncrementingSubWithUnits && !subscriptionWithUnitsExists
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

    if (isIDA) {
        accountTokenSnapshot.totalSubscriptionsWithUnits =
            accountTokenSnapshot.totalSubscriptionsWithUnits +
            totalSubscriptionWithUnitsDelta;
        accountTokenSnapshot.totalApprovedSubscriptions =
            accountTokenSnapshot.totalApprovedSubscriptions +
            totalApprovedSubscriptionsDelta;
    } else {
        accountTokenSnapshot.totalMembershipsWithUnits =
            accountTokenSnapshot.totalMembershipsWithUnits +
            totalSubscriptionWithUnitsDelta;
        accountTokenSnapshot.totalConnectedMemberships =
            accountTokenSnapshot.totalConnectedMemberships +
            totalApprovedSubscriptionsDelta;
    }

    accountTokenSnapshot.isLiquidationEstimateOptimistic =
        accountTokenSnapshot.totalSubscriptionsWithUnits > 0 ||
        accountTokenSnapshot.totalMembershipsWithUnits > 0;
    accountTokenSnapshot.updatedAtTimestamp = block.timestamp;
    accountTokenSnapshot.updatedAtBlockNumber = block.number;
    accountTokenSnapshot.save();

    // update TokenStatistic entity
    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    if (isIDA) {
        tokenStatistic.totalSubscriptionsWithUnits =
            tokenStatistic.totalSubscriptionsWithUnits +
            totalSubscriptionWithUnitsDelta;
        tokenStatistic.totalApprovedSubscriptions =
            tokenStatistic.totalApprovedSubscriptions +
            totalApprovedSubscriptionsDelta;
    } else {
        tokenStatistic.totalMembershipsWithUnits =
            tokenStatistic.totalMembershipsWithUnits +
            totalSubscriptionWithUnitsDelta;
        tokenStatistic.totalConnectedMemberships =
            tokenStatistic.totalConnectedMemberships +
            totalApprovedSubscriptionsDelta;
    }

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

    //////////////// CFA + GDA streamed amounts ////////////////
    const totalAmountStreamedSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalNetFlowRate
        );
    const totalAmountStreamedInSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalInflowRate
        );
    const totalAmountStreamedOutSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalOutflowRate
        );

    // update the totalStreamedUntilUpdatedAt (net)
    accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedUntilUpdatedAt.plus(
            totalAmountStreamedSinceLastUpdatedAt
        );

    // update the totalStreamedUntilUpdatedAt (in)
    accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedInUntilUpdatedAt.plus(
            totalAmountStreamedInSinceLastUpdatedAt
        );

    // update the totalStreamedUntilUpdatedAt (out)
    accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt =
        accountTokenSnapshot.totalAmountStreamedOutUntilUpdatedAt.plus(
            totalAmountStreamedOutSinceLastUpdatedAt
        );

    // update the balance via external call if account has any subscription with more than 0 units
    // or uses the balance delta (which includes amount streamed) and saves the entity
    // we always add the amount streamed in this function
    accountTokenSnapshot = updateATSBalanceAndUpdatedAt(
        accountTokenSnapshot,
        block,
        balanceDelta
            ? balanceDelta.plus(totalAmountStreamedSinceLastUpdatedAt)
            : balanceDelta
    );

    //////////////// CFA streamed amounts ////////////////
    const totalCFAAmountStreamedSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalCFANetFlowRate
        );
    const totalCFAAmountStreamedOutSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            accountTokenSnapshot.updatedAtTimestamp,
            accountTokenSnapshot.totalCFAOutflowRate
        );

    // update the totalCFAStreamedUntilUpdatedAt (net)
    accountTokenSnapshot.totalCFAAmountStreamedUntilUpdatedAt =
        accountTokenSnapshot.totalCFAAmountStreamedUntilUpdatedAt.plus(
            totalCFAAmountStreamedSinceLastUpdatedAt
        );

    // update the totalCFAStreamedUntilUpdatedAt (out)
    accountTokenSnapshot.totalCFAAmountStreamedOutUntilUpdatedAt =
        accountTokenSnapshot.totalCFAAmountStreamedOutUntilUpdatedAt.plus(
            totalCFAAmountStreamedOutSinceLastUpdatedAt
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
 * This function updates the token stats streamed amounts as well as the
 * updatedAtTimestamp and updatedAtBlockNumber.
 * It should always be called with updateATSStreamedAndBalanceUntilUpdatedAt.
 * @param tokenAddress
 * @param block
 */
export function updateTokenStatsStreamedUntilUpdatedAt(
    tokenAddress: Address,
    block: ethereum.Block
): void {
    const tokenStats = getOrInitTokenStatistic(tokenAddress, block);

    //// CFA + GDA streamed amounts ////
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

    //// CFA streamed amounts ////
    const cfaAmountStreamedSinceLastUpdatedAt =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            tokenStats.updatedAtTimestamp,
            tokenStats.totalCFAOutflowRate
        );
    tokenStats.totalCFAAmountStreamedUntilUpdatedAt =
        tokenStats.totalCFAAmountStreamedUntilUpdatedAt.plus(
            cfaAmountStreamedSinceLastUpdatedAt
        );

    tokenStats.updatedAtTimestamp = block.timestamp;
    tokenStats.updatedAtBlockNumber = block.number;
    tokenStats.save();
}

export function updateTokenStatisticStreamData(
    tokenAddress: Address,
    newFlowRate: BigInt,
    flowRateDelta: BigInt,
    depositDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    isCFA: boolean,
    block: ethereum.Block
): void {
    const tokenStatistic = getOrInitTokenStatistic(tokenAddress, block);
    const totalNumberOfActiveStreamsDelta = getActiveStreamsDelta(
        isCreate,
        isDelete
    );
    const totalNumberOfClosedStreamsDelta = getClosedStreamsDelta(isDelete);

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

    tokenStatistic.totalDeposit =
        tokenStatistic.totalDeposit.plus(depositDelta);

    if (isCFA) {
        tokenStatistic.totalCFAOutflowRate = tokenStatistic.totalCFAOutflowRate
            .plus(flowRateDelta)
            .lt(BIG_INT_ZERO)
            ? newFlowRate
            : tokenStatistic.totalCFAOutflowRate.plus(flowRateDelta);

        tokenStatistic.totalCFANumberOfActiveStreams =
            tokenStatistic.totalCFANumberOfActiveStreams +
            totalNumberOfActiveStreamsDelta;

        tokenStatistic.totalCFANumberOfClosedStreams =
            tokenStatistic.totalCFANumberOfClosedStreams +
            totalNumberOfClosedStreamsDelta;

        tokenStatistic.totalCFADeposit =
            tokenStatistic.totalCFADeposit.plus(depositDelta);
    } else {
        tokenStatistic.totalGDAOutflowRate = tokenStatistic.totalGDAOutflowRate
            .plus(flowRateDelta)
            .lt(BIG_INT_ZERO)
            ? newFlowRate
            : tokenStatistic.totalGDAOutflowRate.plus(flowRateDelta);

        tokenStatistic.totalGDANumberOfActiveStreams =
            tokenStatistic.totalGDANumberOfActiveStreams +
            totalNumberOfActiveStreamsDelta;

        tokenStatistic.totalGDANumberOfClosedStreams =
            tokenStatistic.totalGDANumberOfClosedStreams +
            totalNumberOfClosedStreamsDelta;

        tokenStatistic.totalGDADeposit =
            tokenStatistic.totalGDADeposit.plus(depositDelta);
    }
    tokenStatistic.save();
}

/**
 * Updates ATS stream counter data.
 * Must be called after updating streamed amount data for the
 * AccountTokenSnapshot entities.
 */
export function updateSenderATSStreamData(
    senderAddress: Address,
    tokenAddress: Address,
    newFlowRate: BigInt,
    flowRateDelta: BigInt,
    depositDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    isCFA: boolean,
    block: ethereum.Block
): void {
    const totalNumberOfActiveStreamsDelta = getActiveStreamsDelta(
        isCreate,
        isDelete
    );
    const totalNumberOfClosedStreamsDelta = getClosedStreamsDelta(isDelete);
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

    if (isCFA) {
        senderATS.totalCFANetFlowRate =
            senderATS.totalCFANetFlowRate.minus(flowRateDelta);

        // the outflow rate should never go below 0.
        senderATS.totalCFAOutflowRate = senderATS.totalCFAOutflowRate
            .plus(flowRateDelta)
            .lt(BIG_INT_ZERO)
            ? newFlowRate
            : senderATS.totalCFAOutflowRate.plus(flowRateDelta);

        senderATS.totalCFANumberOfActiveStreams =
            senderATS.totalCFANumberOfActiveStreams +
            totalNumberOfActiveStreamsDelta;
        senderATS.activeCFAOutgoingStreamCount =
            senderATS.activeCFAOutgoingStreamCount +
            totalNumberOfActiveStreamsDelta;
        senderATS.inactiveCFAOutgoingStreamCount =
            senderATS.inactiveCFAOutgoingStreamCount +
            totalNumberOfClosedStreamsDelta;

        senderATS.totalCFANumberOfClosedStreams =
            senderATS.totalCFANumberOfClosedStreams +
            totalNumberOfClosedStreamsDelta;
        senderATS.totalCFADeposit =
            senderATS.totalCFADeposit.plus(depositDelta);
    } else {
        // the outflow rate should never go below 0.
        senderATS.totalGDAOutflowRate = senderATS.totalGDAOutflowRate
            .plus(flowRateDelta)
            .lt(BIG_INT_ZERO)
            ? newFlowRate
            : senderATS.totalGDAOutflowRate.plus(flowRateDelta);

        senderATS.totalGDANumberOfActiveStreams =
            senderATS.totalGDANumberOfActiveStreams +
            totalNumberOfActiveStreamsDelta;
        senderATS.activeGDAOutgoingStreamCount =
            senderATS.activeGDAOutgoingStreamCount +
            totalNumberOfActiveStreamsDelta;
        senderATS.inactiveGDAOutgoingStreamCount =
            senderATS.inactiveGDAOutgoingStreamCount +
            totalNumberOfClosedStreamsDelta;

        senderATS.totalGDANumberOfClosedStreams =
            senderATS.totalGDANumberOfClosedStreams +
            totalNumberOfClosedStreamsDelta;
        senderATS.totalGDADeposit =
            senderATS.totalGDADeposit.plus(depositDelta);
    }

    senderATS.save();
}

/**
 * Updates TokenStatistic and AccountTokenSnapshot countable stream
 * data. Must be called after updating streamed amount data for the
 * AccountTokenSnapshot entities.
 */
export function updateReceiverATSStreamData(
    receiverAddress: Address,
    tokenAddress: Address,
    newFlowRate: BigInt,
    flowRateDelta: BigInt,
    isCreate: boolean,
    isDelete: boolean,
    block: ethereum.Block
): void {
    const totalNumberOfActiveStreamsDelta = getActiveStreamsDelta(
        isCreate,
        isDelete
    );
    const totalNumberOfClosedStreamsDelta = getClosedStreamsDelta(isDelete);
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

    receiverATS.totalCFANetFlowRate =
        receiverATS.totalCFANetFlowRate.plus(flowRateDelta);

    receiverATS.totalCFANumberOfActiveStreams =
        receiverATS.totalCFANumberOfActiveStreams +
        totalNumberOfActiveStreamsDelta;
        totalNumberOfClosedStreamsDelta;

    receiverATS.totalCFANumberOfClosedStreams =
        receiverATS.totalCFANumberOfClosedStreams +
        totalNumberOfClosedStreamsDelta;

    receiverATS.save();
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

/**
 * Updates `totalAmountReceivedUntilUpdatedAt` and `poolTotalAmountDistributedUntilUpdatedAt` fields
 * Requires an explicit save on the PoolMember entity.
 * Requires `pool.totalAmountDistributedUntilUpdatedAt` is updated *BEFORE* this function is called.
 * Requires that pool.totalUnits and poolMember.units are updated *AFTER* this function is called.
 * @param pool the pool entity
 * @param poolMember the pool member entity
 * @returns the updated pool member entity to be saved
 */
export function updatePoolMemberTotalAmountUntilUpdatedAtFields(pool: Pool, poolMember: PoolMember): PoolMember {
    let amountReceivedDelta = BIG_INT_ZERO;
    // if the pool member has any units, we calculate the delta
    // otherwise the delta is going to be 0
    if (!poolMember.units.equals(BIG_INT_ZERO)) {
        const distributedAmountDelta = pool.totalAmountDistributedUntilUpdatedAt
            .minus(poolMember.poolTotalAmountDistributedUntilUpdatedAt);

        const isSafeToMultiplyWithoutOverflow = MAX_UINT256.div(poolMember.units).gt(distributedAmountDelta);
        if (isSafeToMultiplyWithoutOverflow) {
            amountReceivedDelta = distributedAmountDelta.times(poolMember.units).div(pool.totalUnits);
        } else {
            amountReceivedDelta = distributedAmountDelta.div(pool.totalUnits).times(poolMember.units);
        }
    }
    poolMember.totalAmountReceivedUntilUpdatedAt =
        poolMember.totalAmountReceivedUntilUpdatedAt.plus(amountReceivedDelta);
    poolMember.poolTotalAmountDistributedUntilUpdatedAt = pool.totalAmountDistributedUntilUpdatedAt;

    return poolMember;
}
