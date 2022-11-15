import { Address, BigInt, ethereum } from "@graphprotocol/graph-ts";
import { createMockedFunction } from "matchstick-as/assembly/index";
import { hostAddress } from "./constants";
import {
    getETHAddress,
    getETHBoolean,
    getETHInt32,
    getETHSignedBigInt,
    getETHString,
    getETHUnsignedBigInt,
} from "./converters";

// Mocked Resolver Functions

/**
 * Creates a mocked Resolver.get(key) function
 * @param resolverAddress
 * @param key the key of the item
 * @param value the expected value
 */
export function mockedResolverGet(
    resolverAddress: string,
    key: string,
    value: string
): void {
    createMockedFunction(
        Address.fromString(resolverAddress),
        "get",
        "get(string):(address)"
    )
        .withArgs([ethereum.Value.fromString(key)])
        .returns([ethereum.Value.fromAddress(Address.fromString(value))]);
}

// Mocked CFAv1 Functions

/**
 * Creates a mocked ConstantFlowAgreementV1.getFlow(token,sender,receiver) function
 * @param cfaV1Address address of cfaV1 contract
 * @param tokenAddress
 * @param senderAddress
 * @param receiverAddress
 * @param expectedTimestamp expected return timestamp (from transaction)
 * @param expectedFlowRate expected flow rate (from input)
 * @param expectedDeposit expected deposit (can be arbitrary, but useful to base off actual)
 * @param expectedOwedDeposit expected owed deposit (0 unless streaming to superApp callback)
 */
export function mockedGetFlow(
    cfaV1Address: string,
    tokenAddress: string,
    senderAddress: string,
    receiverAddress: string,
    expectedTimestamp: BigInt,
    expectedFlowRate: BigInt,
    expectedDeposit: BigInt,
    expectedOwedDeposit: BigInt
): void {
    createMockedFunction(
        Address.fromString(cfaV1Address),
        "getFlow",
        "getFlow(address,address,address):(uint256,int96,uint256,uint256)"
    )
        .withArgs([
            getETHAddress(tokenAddress),
            getETHAddress(senderAddress),
            getETHAddress(receiverAddress),
        ])
        .returns([
            getETHUnsignedBigInt(expectedTimestamp),
            getETHSignedBigInt(expectedFlowRate),
            getETHUnsignedBigInt(expectedDeposit),
            getETHUnsignedBigInt(expectedOwedDeposit),
        ]);
}

// Mocked Host Functions

/**
 * Creates a mocked Superfluid.getAppManifest(address) function
 * @param accountAddress
 * @param expectedIsSuperApp
 * @param expectedIsJailed
 * @param expectedNoopMask
 */
export function mockedGetAppManifest(
    accountAddress: string,
    expectedIsSuperApp: boolean,
    expectedIsJailed: boolean,
    expectedNoopMask: BigInt
): void {
    createMockedFunction(
        Address.fromString(hostAddress),
        "getAppManifest",
        "getAppManifest(address):(bool,bool,uint256)"
    )
        .withArgs([getETHAddress(accountAddress)])
        .returns([
            getETHBoolean(expectedIsSuperApp),
            getETHBoolean(expectedIsJailed),
            getETHUnsignedBigInt(expectedNoopMask),
        ]);
}

// Mocked SuperToken Functions

/**
 * Creates a mocked SuperToken.getHost() function
 * @param tokenAddress
 */
 export function mockedGetHost(tokenAddress: string): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "getHost",
        "getHost():(address)"
    )
        .withArgs([])
        .returns([ethereum.Value.fromAddress(Address.fromString(hostAddress))]);
}

/**
 * Creates a mocked SuperToken.getUnderlyingToken(address) function
 * @param tokenAddress 
 * @param expectedUnderlyingToken 
 */
export function mockedGetUnderlyingToken(
    tokenAddress: string,
    expectedUnderlyingToken: string
): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "getUnderlyingToken",
        "getUnderlyingToken():(address)"
    )
        .withArgs([])
        .returns([getETHAddress(expectedUnderlyingToken)]);
}

/**
 * Creates a mocked SuperToken.realtimeBalanceOf(address,uint256) function
 * @param contractAddress
 * @param accountAddress
 * @param timestamp
 * @param expectedAvailableBalance
 * @param expectedDeposit
 * @param expectedOwedDeposit
 */
export function mockedRealtimeBalanceOf(
    contractAddress: string,
    accountAddress: string,
    timestamp: BigInt,
    expectedAvailableBalance: BigInt,
    expectedDeposit: BigInt,
    expectedOwedDeposit: BigInt
): void {
    createMockedFunction(
        Address.fromString(contractAddress),
        "realtimeBalanceOf",
        "realtimeBalanceOf(address,uint256):(int256,uint256,uint256)"
    )
        .withArgs([
            getETHAddress(accountAddress),
            getETHUnsignedBigInt(timestamp),
        ])
        .returns([
            getETHSignedBigInt(expectedAvailableBalance),
            getETHUnsignedBigInt(expectedDeposit),
            getETHUnsignedBigInt(expectedOwedDeposit),
        ]);
}

/**
 * Creates a mocked SuperToken.name() function
 * @param tokenAddress 
 * @param expectedName 
 */
export function mockedTokenName(
    tokenAddress: string,
    expectedName: string
): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "name",
        "name():(string)"
    )
        .withArgs([])
        .returns([getETHString(expectedName)]);
}

/**
 * Creates a mocked SuperToken.symbol() function
 * @param tokenAddress 
 * @param expectedSymbol 
 */
export function mockedTokenSymbol(
    tokenAddress: string,
    expectedSymbol: string
): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "symbol",
        "symbol():(string)"
    )
        .withArgs([])
        .returns([getETHString(expectedSymbol)]);
}

/**
 * Creates a mocked SuperToken.decimals() function
 * @param tokenAddress 
 * @param expectedDecimals 
 */
export function mockedTokenDecimals(
    tokenAddress: string,
    expectedDecimals: i32
): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "decimals",
        "decimals():(uint8)"
    )
        .withArgs([])
        .returns([getETHInt32(expectedDecimals)]);
}

/**
 * Creates a mocked SuperToken.totalSupply() function
 * @param tokenAddress 
 * @param expectedTotalSupply 
 */
export function mockedTokenTotalSupply(
    tokenAddress: string,
    expectedTotalSupply: BigInt
): void {
    createMockedFunction(
        Address.fromString(tokenAddress),
        "totalSupply",
        "totalSupply():(uint256)"
    )
        .withArgs([])
        .returns([getETHUnsignedBigInt(expectedTotalSupply)]);
}