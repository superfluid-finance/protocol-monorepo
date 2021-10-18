import { BigInt, Bytes, ethereum, Address, log } from "@graphprotocol/graph-ts";
import { ISuperToken as SuperToken } from "../generated/templates/SuperToken/ISuperToken";
import { TestResolver } from "../generated/ResolverV1/TestResolver";
import { StreamRevision, IndexSubscription, Token } from "../generated/schema";

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

export function getIsListedToken(
    token: Token,
    tokenAddress: Address,
    resolverAddress: Address,
    symbol: string
): Token {
    let resolverContract = TestResolver.bind(resolverAddress);
    let result = resolverContract.try_get(`supertokens.v1.${symbol}`);
    let superTokenAddress = result.reverted ? new Address(0) : result.value;
    token.isListed = tokenAddress.toHex() == superTokenAddress.toHex();
    return token as Token;
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
