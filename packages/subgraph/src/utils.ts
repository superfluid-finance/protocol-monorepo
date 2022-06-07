import {Address, BigInt, Bytes, ethereum, log,} from "@graphprotocol/graph-ts";
import {ISuperToken as SuperToken} from "../generated/templates/SuperToken/ISuperToken";
import {Resolver} from "../generated/ResolverV1/Resolver";
import {IndexSubscription, StreamRevision, Token, TokenStatistic,} from "../generated/schema";

/**************************************************************************
 * Constants
 *************************************************************************/
export const BIG_INT_ZERO = BigInt.fromI32(0);
export const BIG_INT_ONE = BigInt.fromI32(1);
export const ZERO_ADDRESS = Address.zero();
export let MAX_FLOW_RATE = BigInt.fromI32(2).pow(95).minus(BigInt.fromI32(1));
export const ORDER_MULTIPLIER = BigInt.fromI32(10000);
export const MAX_SAFE_SECONDS = BigInt.fromI64(8640000000000); //In seconds, https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date#the_ecmascript_epoch_and_timestamps
/**************************************************************************
 * Convenience Conversions
 *************************************************************************/
export function bytesToAddress(bytes: Bytes): Address {
    return Address.fromBytes(bytes);
}

/**************************************************************************
 * Event entities util functions
 *************************************************************************/

export function createEventID(
    eventName: string,
    event: ethereum.Event
): string {
    return (
        eventName +
        "-" +
        event.transaction.hash.toHexString() +
        "-" +
        event.logIndex.toString()
    );
}

/**************************************************************************
 * HOL entities util functions
 *************************************************************************/

export function getTokenInfoAndReturn(
    token: Token,
    tokenAddress: Address
): Token {
    let tokenContract = SuperToken.bind(tokenAddress);
    let underlyingAddressResult = tokenContract.try_getUnderlyingToken();
    let nameResult = tokenContract.try_name();
    let symbolResult = tokenContract.try_symbol();
    let decimalsResult = tokenContract.try_decimals();
    token.underlyingAddress = underlyingAddressResult.reverted
        ? ZERO_ADDRESS
        : underlyingAddressResult.value;
    token.name = nameResult.reverted ? "" : nameResult.value;
    token.symbol = symbolResult.reverted ? "" : symbolResult.value;
    token.decimals = decimalsResult.reverted ? 0 : decimalsResult.value;
    return token;
}

export function getIsListedToken(
    token: Token,
    tokenAddress: Address,
    resolverAddress: Address
): Token {
    let resolverContract = Resolver.bind(resolverAddress);
    let version =
        resolverAddress.toHex() == "0xe7f1725e7734ce288f8367e1bb143e90bb3f0512"
            ? "test"
            : "v1";
    let result = resolverContract.try_get(
        "supertokens." + version + "." + token.symbol
    );
    let superTokenAddress = result.reverted ? ZERO_ADDRESS : result.value;
    token.isListed = tokenAddress.toHex() == superTokenAddress.toHex();
    return token as Token;
}

export function updateTotalSupplyForNativeSuperToken(
    token: Token,
    tokenStatistic: TokenStatistic,
    tokenAddress: Address
): TokenStatistic {
    if (
        Address.fromBytes(token.underlyingAddress).equals(ZERO_ADDRESS) &&
        tokenStatistic.totalSupply.equals(BIG_INT_ZERO)
    ) {
        let tokenContract = SuperToken.bind(tokenAddress);
        let totalSupplyResult = tokenContract.try_totalSupply();
        if (totalSupplyResult.reverted) {
            return tokenStatistic;
        }
        tokenStatistic.totalSupply = totalSupplyResult.value;
    }
    return tokenStatistic;
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

// Get Higher Order Entity ID functions
// CFA Higher Order Entity
export function getStreamRevisionID(
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address
): string {
    return (
        senderAddress.toHex() +
        "-" +
        receiverAddress.toHex() +
        "-" +
        tokenAddress.toHex()
    );
}

export function getStreamID(
    senderAddress: Address,
    receiverAddress: Address,
    tokenAddress: Address,
    revisionIndex: number
): string {
    return (
        getStreamRevisionID(senderAddress, receiverAddress, tokenAddress) +
        "-" +
        revisionIndex.toString()
    );
}

export function getStreamPeriodID(
    streamId: string,
    periodRevisionIndex: number
): string {
    return streamId + "-" + periodRevisionIndex.toString();
}

export function getFlowOperatorID(
    flowOperatorAddress: Address,
    tokenAddress: Address,
    senderAddress: Address
): string {
    return (
        flowOperatorAddress.toHex() +
        "-" +
        tokenAddress.toHex() +
        "-" +
        senderAddress.toHex()
    );
}

// IDA Higher Order Entity
export function getSubscriptionID(
    subscriberAddress: Address,
    publisherAddress: Address,
    tokenAddress: Address,
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
    publisherAddress: Address,
    tokenAddress: Address,
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

// Get Aggregate ID functions
export function getAccountTokenSnapshotID(
    accountAddress: Address,
    tokenAddress: Address
): string {
    return accountAddress.toHex() + "-" + tokenAddress.toHex();
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

export function getAmountStreamedSinceLastUpdatedAt(
    currentTime: BigInt,
    lastUpdatedTime: BigInt,
    previousTotalOutflowRate: BigInt
): BigInt {
    let timeDelta = currentTime.minus(lastUpdatedTime);
    return timeDelta.times(previousTotalOutflowRate);
}

/**
 * calculateMaybeCriticalAtTimestamp will return optimistic date based on updatedAtTimestamp, balanceUntilUpdatedAt and totalNetFlowRate.
 * @param updatedAtTimestamp
 * @param balanceUntilUpdatedAt
 * @param totalNetFlowRate
 * @param previousMaybeCriticalAtTimestamp
 */

export function calculateMaybeCriticalAtTimestamp(
    updatedAtTimestamp: BigInt,
    balanceUntilUpdatedAt: BigInt,
    totalNetFlowRate: BigInt,
    previousMaybeCriticalAtTimestamp: BigInt | null,
): BigInt | null {
    // When the flow rate is not negative then there's no way to have a critical balance timestamp anymore.
    if (totalNetFlowRate.ge(BIG_INT_ZERO)) return null;

    // When there's no balance then that either means:
    // 1. account is already critical and we keep the existing timestamp when the liquidations supposedly started
    // 2. it's a new account without a critical balance timestamp to begin with
    if (balanceUntilUpdatedAt.le(BIG_INT_ZERO)) return previousMaybeCriticalAtTimestamp;

    const secondsUntilCritical = balanceUntilUpdatedAt.div(totalNetFlowRate.abs());
    const calculatedCriticalTimestamp = updatedAtTimestamp.plus(secondsUntilCritical);
    if (calculatedCriticalTimestamp.gt(MAX_SAFE_SECONDS)) {
        return MAX_SAFE_SECONDS;
    }
    return calculatedCriticalTimestamp;
}

/**
 * getOrder calculate order based on {blockNumber.times(10000).plus(logIndex)}.
 * @param blockNumber
 * @param logIndex
 */
export function getOrder(
    blockNumber: BigInt,
    logIndex: BigInt,
): BigInt {
    return blockNumber.times(ORDER_MULTIPLIER).plus(logIndex);
}

/**************************************************************************
 * Log entities util functions
 *************************************************************************/

export function createLogID(
    logPrefix: string,
    accountTokenSnapshotId: string,
    event: ethereum.Event,
): string {
    return (
        logPrefix
        + "-" +
        accountTokenSnapshotId
        + "-" +
        event.transaction.hash.toHexString()
        + "-" +
        event.logIndex.toString()
    );
}
