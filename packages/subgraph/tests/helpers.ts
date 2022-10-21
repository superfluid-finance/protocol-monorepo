import { Address, ethereum } from "@graphprotocol/graph-ts";
import { Token } from "../generated/schema";
import { getNativeAssetSuperTokenAddress } from "../src/addresses";

/**
 * Creates a SuperToken entity
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
    let token = Token.load(tokenId);

    let currentTimestamp = block.timestamp;

    token = new Token(tokenId);
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
