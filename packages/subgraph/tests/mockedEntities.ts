import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { StreamRevision, Token } from "../generated/schema";
import { getNativeAssetSuperTokenAddress } from "../src/addresses";

/**
 * Creates a SuperToken entity
 * Note: this is needed otherwise we get a null error when we try
 *       run getIsListedToken and it tries to access token.symbol
 * @param tokenAddress the address of the token
 * @param block transaction block object
 * @param decimals number of decimals
 * @param name token name
 * @param symbol token symbol
 * @param isListed whether the token is listed
 * @param underlyingAddress the underlying token address
 * @returns Token
 */
export function createSuperToken(
    tokenAddress: Address,
    block: ethereum.Block,
    decimals: i32,
    name: string,
    symbol: string,
    isListed: boolean,
    underlyingAddress: Address
): Token {
    const tokenId = tokenAddress.toHex();
    let currentTimestamp = block.timestamp;

    const token = new Token(tokenId);
    token.createdAtTimestamp = currentTimestamp;
    token.createdAtBlockNumber = block.number;
    token.decimals = decimals;
    token.name = name;
    token.symbol = symbol;
    token.isSuperToken = true;

    const nativeAssetSuperTokenAddress = getNativeAssetSuperTokenAddress();
    token.isNativeAssetSuperToken = tokenAddress.equals(
        nativeAssetSuperTokenAddress
    );
    token.isListed = isListed;

    token.underlyingAddress = underlyingAddress;
    token.underlyingToken = underlyingAddress.toHex();
    
    token.save();
    return token as Token;
}

/**
 * Creates a StreamRevision entity
 * @param flowId 
 * @param tokenAddress 
 * @param deposit 
 * @param revisionIndex 
 * @param periodRevisionIndex 
 * @returns StreamRevision
 */
export function createStreamRevision(
    flowId: string,
    tokenAddress: string,
    deposit: BigInt,
    revisionIndex: i32,
    periodRevisionIndex: i32,
): StreamRevision {
    const id = flowId + "-" + tokenAddress;

    const streamRevision = new StreamRevision(id);
    streamRevision.deposit = deposit;
    streamRevision.revisionIndex = revisionIndex;
    streamRevision.periodRevisionIndex =  periodRevisionIndex;

    streamRevision.save();
    return streamRevision;
}