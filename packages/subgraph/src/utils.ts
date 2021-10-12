import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { ISuperfluid as Superfluid } from "../generated/Host/ISuperfluid";
import { IResolver } from "../generated/templates/SuperToken/IResolver";
import {
    Account,
    Index,
    AccountTokenSnapshot,
    Stream,
    StreamRevision,
    IndexSubscription,
    TokenStatistic,
    Token,
} from "../generated/schema";
import { SuperToken as SuperTokenTemplate } from "../generated/templates";

// TODO: GET DATA 100% ACCURATE
// THEN: WORK ON MAPPING OPTIMIZATION - this is mainly contract calls
// and .load (kinda)
// e.g. don't save in the modifier functions, instead let it take the object in
// and always return the object - always save outside of the util functions at
// the mapping level

/**************************************************************************
 * Constants
 *************************************************************************/

export let BIG_INT_ZERO = BigInt.fromI32(0);
export let BIG_INT_ONE = BigInt.fromI32(1);

// the host address will be consistent as long as you use the
// first account retrieved by hardhat's ethers.getSigners():
// 0xf39fd6e51aad88f6f4ce6ab8827279cfffb92266 and the nonce is 0
export let GANACHE_HOST_ADDRESS = "0xa513E6E4b8f2a923D98304ec87F64353C4D5C853"; // for testing
export let GOERLI_HOST_ADDRESS = "0x22ff293e14F1EC3A09B137e9e06084AFd63adDF9";
export let KOVAN_HOST_ADDRESS = "0xF0d7d1D47109bA426B9D8A3Cde1941327af1eea3";
export let MATIC_HOST_ADDRESS = "0x3E14dC1b13c488a8d5D310918780c983bD5982E7";
export let MUMBAI_HOST_ADDRESS = "0xEB796bdb90fFA0f28255275e16936D25d3418603";
export let RINKEBY_HOST_ADDRESS = "0xeD5B5b32110c3Ded02a07c8b8e97513FAfb883B6";
export let ROPSTEN_HOST_ADDRESS = "0xF2B4E81ba39F5215Db2e05B2F66f482BB8e87FD2";
export let XDAI_HOST_ADDRESS = "0x2dFe937cD98Ab92e59cF3139138f18c823a4efE7";

export let GANACHE_RESOLVER_ADDRESS =
    "0xe7f1725E7734CE288F8367e1Bb143E90bb3F0512";
export let GOERLI_RESOLVER_ADDRESS =
    "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E";
export let KOVAN_RESOLVER_ADDRESS =
    "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B";
export let MATIC_RESOLVER_ADDRESS =
    "0xE0cc76334405EE8b39213E620587d815967af39C";
export let MUMBAI_RESOLVER_ADDRESS =
    "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3";
export let RINKEBY_RESOLVER_ADDRESS =
    "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A";
export let ROPSTEN_RESOLVER_ADDRESS =
    "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a";
export let XDAI_RESOLVER_ADDRESS = "0xD2009765189164b495c110D61e4D301729079911";
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

export function getTokenInfoAndReturn(
    token: Token,
    tokenAddress: Address,
    resolverAddress: Address
): Token {
    let tokenContract = SuperToken.bind(tokenAddress);
    let resolverContract = IResolver.bind(resolverAddress);
    let underlyingAddressResult = tokenContract.try_getUnderlyingToken();
    let nameResult = tokenContract.try_name();
    let symbolResult = tokenContract.try_symbol();
    // let isListedResult = resolverContract.try_get(
    //     `supertokens.v1.${symbolResult.value}`
    // );
    token.underlyingAddress = underlyingAddressResult.reverted
        ? new Address(0)
        : underlyingAddressResult.value;
    token.name = nameResult.reverted ? "" : nameResult.value;
    token.symbol = symbolResult.reverted ? "" : symbolResult.value;
    token.isListed = false;
    // let superTokenAddress = isListedResult.reverted
    //     ? new Address(0)
    //     : isListedResult.value;
    // token.isListed = tokenAddress.toHex() == superTokenAddress.toHex();
    return token;
}

/**
 * Creates a HOL Token (SuperToken) entity if non exists.
 * We also create token stats in here if it doesn't exist yet.
 * @param tokenAddress
 * @param resolverAddress
 * @param block
 * @returns
 */
export function getOrInitSuperToken(
    tokenAddress: Address,
    resolverAddress: Address,
    block: ethereum.Block
): Token {
    let tokenId = tokenAddress.toHex();
    let token = Token.load(tokenId);
    let currentTimestamp = block.timestamp;

    if (token == null) {
        token = new Token(tokenId);
        token.createdAtTimestamp = currentTimestamp;
        token.createdAtBlockNumber = block.number;
        token.isSuperToken = true;
        token = getTokenInfoAndReturn(
            token as Token,
            tokenAddress,
            resolverAddress
        );
        token.save();

        // Note: we initalize and create tokenStatistic whenever we create a
        // token as well.
        let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
        tokenStatistic.save();

        // Note: this is necessary otherwise we will not be able to capture
        // template data source events.
        SuperTokenTemplate.create(tokenAddress);

        let underlyingAddress = token.underlyingAddress;

        // If the token has an underlying ERC20, we create a token entity for it.
        let underlyingToken = Token.load(token.underlyingAddress.toHex());
        if (
            underlyingAddress.notEqual(new Address(0)) &&
            underlyingToken == null
        ) {
            getOrInitToken(underlyingAddress as Address, block);
        }

        return token as Token;
    }

    // // we must handle the case when the native token hasn't been initialized
    // // there is no name/symbol, but this may occur later
    if (token.name.length == 0 || token.symbol.length == 0) {
        token = getTokenInfoAndReturn(
            token as Token,
            tokenAddress,
            resolverAddress
        );
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
    token.createdAtTimestamp = block.timestamp;
    token.createdAtBlockNumber = block.number;
    token.isSuperToken = false;
    token.isListed = false;
    token = getTokenInfoAndReturn(token as Token, tokenAddress, new Address(0));
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
    }
    return streamRevision as StreamRevision;
}

/**
 * Gets or initializes a Stream, always sets the updatedAt.
 * @param hostAddress
 * @param resolverAddress
 * @param senderAddress
 * @param receiverAddress
 * @param tokenAddress
 * @param block
 * @returns
 */
export function getOrInitStream(
    hostAddress: Address,
    resolverAddress: Address,
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
        getOrInitSuperToken(tokenAddress, resolverAddress, block);
    }
    return stream as Stream;
}

/**
 * Gets or initializes an Index, always sets the updatedAt.
 * @param hostAddress
 * @param resolverAddress
 * @param publisherAddress
 * @param tokenAddress
 * @param indexId
 * @param block
 * @param indexCreatedId
 * @returns
 */
export function getOrInitIndex(
    hostAddress: Address,
    resolverAddress: Address,
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

        getOrInitAccount(hostAddress, publisherAddress, block);

        // NOTE: we must check if token exists and create here
        // if not. for SETH tokens (e.g. ETH, MATIC, xDAI)
        getOrInitSuperToken(tokenAddress, resolverAddress, block);
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
    hostAddress: Address,
    resolverAddress: Address,
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
            hostAddress,
            resolverAddress,
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

        getOrInitAccount(hostAddress, subscriberAddress, block);
    }
    subscription.updatedAtTimestamp = currentTimestamp;
    subscription.updatedAtBlockNumber = block.number;
    return subscription as IndexSubscription;
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
    // filter out 0 address accounts
    if (accountAddress.equals(new Address(0))) {
        return;
    }
    let account = getOrInitAccount(hostAddress, accountAddress, block);
    account.updatedAtTimestamp = block.timestamp;
    account.updatedAtBlockNumber = block.number;
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

export function getSubscriptionID(
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

/**
 * If your units get set to 0, you will still have a subscription
 * entity, but your subscription technically no longer exists.
 * Similarly, you may be approved, but the subscription by this
 * definition does not exist.
 * @param id
 * @returns
 */
export function subscriptionExists(id: string): boolean {
    let subscription = IndexSubscription.load(id);
    return subscription != null && subscription.units.gt(BIG_INT_ZERO);
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
export function updateATSBalanceAndUpdatedAt(
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
    accountTokenSnapshot.updatedAtBlockNumber = block.number;
    accountTokenSnapshot.save();
}

function getAmountStreamedSinceLastUpdatedAt(
    currentTime: BigInt,
    lastUpdatedTime: BigInt,
    previousTotalOutflowRate: BigInt
): BigInt {
    let timeDelta = currentTime.minus(lastUpdatedTime);
    return timeDelta.times(previousTotalOutflowRate);
}

// TODO: it may make sense to combine this with getting update balance right after.
/**
 * @dev Must call before updatedAt is updated.
 * @param accountId
 * @param tokenId
 * @param block
 */
export function updateATSStreamedUntilUpdatedAt(
    accountId: string,
    tokenId: string,
    block: ethereum.Block
): void {
    let ats = getOrInitAccountTokenSnapshot(accountId, tokenId, block);
    let amountStreamedSinceLastUpdatedAt = getAmountStreamedSinceLastUpdatedAt(
        block.timestamp,
        ats.updatedAtTimestamp,
        ats.totalOutflowRate
    );
    ats.totalAmountStreamedUntilUpdatedAt =
        ats.totalAmountStreamedUntilUpdatedAt.plus(
            amountStreamedSinceLastUpdatedAt
        );
    ats.save();
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
    let senderATSAmountStreamedSinceLastUpdate =
        getAmountStreamedSinceLastUpdatedAt(
            block.timestamp,
            senderATS.updatedAtTimestamp,
            senderATS.totalOutflowRate
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
    senderATS.totalAmountStreamedUntilUpdatedAt =
        senderATS.totalAmountStreamedUntilUpdatedAt.plus(
            senderATSAmountStreamedSinceLastUpdate
        );

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
    fromAccountTokenSnapshot.updatedAtBlockNumber = block.number;
    fromAccountTokenSnapshot.save();

    let tokenStatistic = getOrInitTokenStatistic(tokenId, block);
    tokenStatistic.totalAmountTransferredUntilUpdatedAt =
        tokenStatistic.totalAmountTransferredUntilUpdatedAt.plus(value);
    tokenStatistic.save();
}
