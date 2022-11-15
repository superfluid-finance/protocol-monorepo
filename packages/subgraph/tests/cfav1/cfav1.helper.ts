import { Address, BigInt, Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    FlowUpdated,
    FlowUpdatedExtension,
    FlowOperatorUpdated,
} from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import { BIG_INT_ZERO } from "../../src/utils";
import { FAKE_INITIAL_BALANCE, FAKE_SUPER_TOKEN_TOTAL_SUPPLY, resolverAddress } from "../constants";
import {
    getAddressEventParam,
    getBigIntEventParam,
    getBytesEventParam,
    getI32EventParam,
} from "../converters";
import {
    mockedGetAppManifest,
    mockedGetFlow,
    mockedGetHost,
    mockedGetUnderlyingToken,
    mockedRealtimeBalanceOf,
    mockedResolverGet,
    mockedTokenDecimals,
    mockedTokenName,
    mockedTokenSymbol,
    mockedTokenTotalSupply,
} from "../mockedFunctions";

export function getFlowOperatorId(
    flowOperatorAddress: string,
    tokenAddress: string,
    senderAddress: string
): string {
    return flowOperatorAddress + "-" + tokenAddress + "-" + senderAddress;
}

export function createFlowUpdatedEvent(
    token: string,
    sender: string,
    receiver: string,
    flowRate: BigInt,
    totalSenderFlowRate: BigInt,
    totalReceiverFlowRate: BigInt,
    userData: Bytes
): FlowUpdated {
    const newFlowUpdatedEvent = changetype<FlowUpdated>(newMockEvent());
    newFlowUpdatedEvent.parameters = new Array();
    newFlowUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newFlowUpdatedEvent.parameters.push(getAddressEventParam("sender", sender));
    newFlowUpdatedEvent.parameters.push(getAddressEventParam("receiver", receiver));
    newFlowUpdatedEvent.parameters.push(getBigIntEventParam("flowRate", flowRate));
    newFlowUpdatedEvent.parameters.push(getBigIntEventParam("totalSenderFlowRate", totalSenderFlowRate));
    newFlowUpdatedEvent.parameters.push(getBigIntEventParam("totalReceiverFlowRate", totalReceiverFlowRate));
    newFlowUpdatedEvent.parameters.push(getBytesEventParam("userData", userData));

    return newFlowUpdatedEvent;
}

export function createFlowUpdatedExtensionEvent(
    flowOperator: string,
    deposit: BigInt
): FlowUpdatedExtension {
    const newFlowUpdatedExtensionEvent = changetype<FlowUpdatedExtension>(
        newMockEvent()
    );
    newFlowUpdatedExtensionEvent.parameters = new Array();
    newFlowUpdatedExtensionEvent.parameters.push(getAddressEventParam("flowOperator", flowOperator));
    newFlowUpdatedExtensionEvent.parameters.push(getBigIntEventParam("deposit", deposit));

    return newFlowUpdatedExtensionEvent;
}

export function createFlowOperatorUpdatedEvent(
    token: string,
    sender: string,
    flowOperator: string,
    permissions: i32,
    flowRateAllowance: BigInt
): FlowOperatorUpdated {
    const newFlowOperatorUpdatedEvent = changetype<FlowOperatorUpdated>(
        newMockEvent()
    );
    newFlowOperatorUpdatedEvent.parameters = new Array();
    newFlowOperatorUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newFlowOperatorUpdatedEvent.parameters.push(getAddressEventParam("sender", sender));
    newFlowOperatorUpdatedEvent.parameters.push(getAddressEventParam("flowOperator", flowOperator));
    newFlowOperatorUpdatedEvent.parameters.push(getI32EventParam("permissions", permissions));
    newFlowOperatorUpdatedEvent.parameters.push(getBigIntEventParam("flowRateAllowance", flowRateAllowance));

    return newFlowOperatorUpdatedEvent;
}

/**
 * Creates the necessary mocked functions in order for the handleFlowUpdated mapping function to work as expected.
 * @param flowUpdatedEvent 
 * @param superToken 
 * @param decimals 
 * @param tokenName 
 * @param tokenSymbol 
 * @param underlyingAddress 
 * @param expectedDeposit 
 * @param expectedOwedDeposit 
 */
export function mockedHandleFlowUpdatedRPCCalls(
    flowUpdatedEvent: FlowUpdated,
    superToken: string,
    decimals: i32,
    tokenName: string,
    tokenSymbol: string,
    underlyingAddress: Address,
    expectedDeposit: BigInt,
    expectedOwedDeposit: BigInt
): void {
    const sender = flowUpdatedEvent.params.sender.toHex();
    const receiver = flowUpdatedEvent.params.receiver.toHex();
    const flowRate = flowUpdatedEvent.params.flowRate;

    // tokenHasValidHost => tokenContract.try_getHost()
    mockedGetHost(superToken);

    // cfaContract.try_GetFlow(token,sender,receiver)
    mockedGetFlow(
        flowUpdatedEvent.address.toHexString(),
        superToken,
        flowUpdatedEvent.params.sender.toHexString(),
        flowUpdatedEvent.params.receiver.toHexString(),
        flowUpdatedEvent.block.timestamp,
        flowRate,
        expectedDeposit,
        expectedOwedDeposit
    );


    // getOrInitStream(event) => getOrInitAccount(sender) => host.try_getAppManifest(sender)
    mockedGetAppManifest(sender, false, false, BIG_INT_ZERO);
    // getOrInitStream(event) => getOrInitAccount(receiver) => host.try_getAppManifest(receiver)
    mockedGetAppManifest(receiver, false, false, BIG_INT_ZERO);

    // [START] getOrInitStream(event) => getOrInitSuperToken(token, block) => handleTokenRPCCalls(token, resolverAddress) => 
    
    // [START] getTokenInfoAndReturn => 
    // token.try_getUnderlyingToken()
    mockedGetUnderlyingToken(superToken, underlyingAddress.toHexString());
    // token.try_name()
    mockedTokenName(superToken, tokenName);
    // token.try_symbol()
    mockedTokenSymbol(superToken, tokenSymbol);
    // token.try_decimals()
    mockedTokenDecimals(superToken, decimals);
    // [END] getTokenInfoAndReturn

    // getIsListedToken(token, resolver) => resolver.try_get(key)
    mockedResolverGet(
        resolverAddress,
        "supertokens.test." + tokenSymbol,
        superToken
    );
    // [END] getOrInitStream(event) => getOrInitSuperToken(token, block) => handleTokenRPCCalls(token, resolverAddress)

    // getOrInitStream(event) => getOrInitSuperToken(token, block) => updateTotalSupplyForNativeSuperToken(token, tokenStatistic, tokenAddress)
    mockedTokenTotalSupply(superToken, FAKE_SUPER_TOKEN_TOTAL_SUPPLY);

    // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(sender)
    mockedRealtimeBalanceOf(
        superToken,
        sender,
        flowUpdatedEvent.block.timestamp,
        FAKE_INITIAL_BALANCE.minus(flowRate),
        flowRate,
        BIG_INT_ZERO
    );
    // updateATSStreamedAndBalanceUntilUpdatedAt => updateATSBalanceAndUpdatedAt => try_realtimeBalanceOf(receiver)
    mockedRealtimeBalanceOf(
        superToken,
        receiver,
        flowUpdatedEvent.block.timestamp,
        FAKE_INITIAL_BALANCE,
        BIG_INT_ZERO,
        BIG_INT_ZERO
    );
}
