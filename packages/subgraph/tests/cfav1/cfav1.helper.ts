import { BigInt, Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    FlowUpdated,
    FlowUpdatedExtension,
    FlowOperatorUpdated,
} from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import {
    getAddressEventParam,
    getBigIntEventParam,
    getBytesEventParam,
    getI32EventParam,
} from "../converters";

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