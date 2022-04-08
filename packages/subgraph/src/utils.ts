import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { Resolver } from "../generated/ResolverV1/Resolver";
import {
    StreamRevision,
    IndexSubscription,
    Token,
    TokenStatistic,
} from "../generated/schema";

/**************************************************************************
 * Constants
 *************************************************************************/

export let BIG_INT_ZERO = BigInt.fromI32(0);
export let BIG_INT_ONE = BigInt.fromI32(1);
export let ZERO_ADDRESS = Address.fromString(
    "0x0000000000000000000000000000000000000000"
);

/**************************************************************************
 * Event entities util functions
 *************************************************************************/

export function createEventID(
    eventName: string,
    event: ethereum.Event
): string {
    return eventName
        .concat("-")
        .concat(event.transaction.hash.toHexString())
        .concat("-")
        .concat(event.logIndex.toString());
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
        ? new Address(0)
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
        "supertokens.".concat(version).concat(".").concat(token.symbol)
    );
    let superTokenAddress = result.reverted ? new Address(0) : result.value;
    token.isListed = tokenAddress.toHex() == superTokenAddress.toHex();
    return token as Token;
}

export function updateTotalSupplyForNativeSuperToken(
    token: Token,
    tokenStatistic: TokenStatistic,
    tokenAddress: Address
): TokenStatistic {
    if (token.underlyingAddress.toHex() == "0x0000000000000000000000000000000000000000" &&
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

// Get HOL ID functions
export function getStreamRevisionPrefix(
    senderId: string,
    receiverId: string,
    tokenId: string
): string {
    return senderId.concat("-").concat(receiverId).concat("-").concat(tokenId);
}

export function getStreamID(
    senderId: string,
    receiverId: string,
    tokenId: string,
    revisionIndex: number
): string {
    return getStreamRevisionPrefix(senderId, receiverId, tokenId)
        .concat("-")
        .concat(revisionIndex.toString());
}

export function getStreamPeriodID(
    streamId: string,
    periodRevisionIndex: number
): string {
    return streamId.concat("-").concat(periodRevisionIndex.toString());
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

export function getAmountStreamedSinceLastUpdatedAt(
    currentTime: BigInt,
    lastUpdatedTime: BigInt,
    previousTotalOutflowRate: BigInt
): BigInt {
    let timeDelta = currentTime.minus(lastUpdatedTime);
    return timeDelta.times(previousTotalOutflowRate);
}

// Get Aggregate ID functions
export function getAccountTokenSnapshotID(
    accountId: string,
    tokenId: string
): string {
    return accountId.concat("-").concat(tokenId);
}
