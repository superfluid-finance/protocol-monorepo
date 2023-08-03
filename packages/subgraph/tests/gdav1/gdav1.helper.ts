import { newMockEvent } from "matchstick-as";
import {
    BufferAdjusted,
    FlowDistributionUpdated,
    InstantDistributionUpdated,
    PoolConnectionUpdated,
    PoolCreated,
} from "../../generated/GeneralDistributionAgreementV1/IGeneralDistributionAgreementV1";
import {
    DistributionClaimed,
    MemberUnitsUpdated,
} from "../../generated/GeneralDistributionAgreementV1/ISuperfluidPool";
import { getAddressEventParam, getBigIntEventParam, getBooleanEventParam } from "../converters";
import { BigInt } from "@graphprotocol/graph-ts";

// Mock Event Creators
export function createPoolCreatedEvent(token: string, admin: string, pool: string): PoolCreated {
    const newPoolCreatedEvent = changetype<PoolCreated>(newMockEvent());
    newPoolCreatedEvent.parameters = new Array();
    newPoolCreatedEvent.parameters.push(getAddressEventParam("token", token));
    newPoolCreatedEvent.parameters.push(getAddressEventParam("admin", admin));
    newPoolCreatedEvent.parameters.push(getAddressEventParam("pool", pool));

    return newPoolCreatedEvent;
}

export function createPoolConnectionUpdatedEvent(
    token: string,
    connected: boolean,
    pool: string,
    poolMember: string
): PoolConnectionUpdated {
    const newPoolConnectionUpdatedEvent = changetype<PoolConnectionUpdated>(newMockEvent());
    newPoolConnectionUpdatedEvent.parameters = new Array();
    newPoolConnectionUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newPoolConnectionUpdatedEvent.parameters.push(getBooleanEventParam("connected", connected));
    newPoolConnectionUpdatedEvent.parameters.push(getAddressEventParam("pool", pool));
    newPoolConnectionUpdatedEvent.parameters.push(getAddressEventParam("poolMember", poolMember));

    return newPoolConnectionUpdatedEvent;
}

export function createBufferAdjustedEvent(
    token: string,
    bufferDelta: BigInt,
    newBufferAmount: BigInt,
    totalBufferAmount: BigInt,
    pool: string,
    poolDistributor: string
): BufferAdjusted {
    const newBufferAdjustedEvent = changetype<BufferAdjusted>(newMockEvent());
    newBufferAdjustedEvent.parameters = new Array();
    newBufferAdjustedEvent.parameters.push(getAddressEventParam("token", token));
    newBufferAdjustedEvent.parameters.push(getBigIntEventParam("bufferDelta", bufferDelta));
    newBufferAdjustedEvent.parameters.push(getBigIntEventParam("newBufferAmount", newBufferAmount));
    newBufferAdjustedEvent.parameters.push(getBigIntEventParam("totalBufferAmount", totalBufferAmount));
    newBufferAdjustedEvent.parameters.push(getAddressEventParam("pool", pool));
    newBufferAdjustedEvent.parameters.push(getAddressEventParam("poolDistributor", poolDistributor));

    return newBufferAdjustedEvent;
}

export function createInstantDistributionUpdatedEvent(
    token: string,
    operator: string,
    requestedAmount: BigInt,
    actualAmount: BigInt,
    pool: string,
    poolDistributor: string
): InstantDistributionUpdated {
    const newInstantDistributionUpdatedEvent = changetype<InstantDistributionUpdated>(newMockEvent());
    newInstantDistributionUpdatedEvent.parameters = new Array();
    newInstantDistributionUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newInstantDistributionUpdatedEvent.parameters.push(getAddressEventParam("operator", operator));
    newInstantDistributionUpdatedEvent.parameters.push(getBigIntEventParam("requestedAmount", requestedAmount));
    newInstantDistributionUpdatedEvent.parameters.push(getBigIntEventParam("actualAmount", actualAmount));
    newInstantDistributionUpdatedEvent.parameters.push(getAddressEventParam("pool", pool));
    newInstantDistributionUpdatedEvent.parameters.push(getAddressEventParam("poolDistributor", poolDistributor));

    return newInstantDistributionUpdatedEvent;
}

export function createFlowDistributionUpdatedEvent(
    token: string,
    operator: string,
    oldFlowRate: BigInt,
    newDistributorToPoolFlowRate: BigInt,
    newTotalDistributionFlowRate: BigInt,
    adjustmentFlowRecipient: string,
    adjustmentFlowRate: BigInt,
    pool: string,
    poolDistributor: string
): FlowDistributionUpdated {
    const newFlowDistributionUpdatedEvent = changetype<FlowDistributionUpdated>(newMockEvent());
    newFlowDistributionUpdatedEvent.parameters = new Array();
    newFlowDistributionUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newFlowDistributionUpdatedEvent.parameters.push(getAddressEventParam("operator", operator));
    newFlowDistributionUpdatedEvent.parameters.push(getBigIntEventParam("oldFlowRate", oldFlowRate));
    newFlowDistributionUpdatedEvent.parameters.push(
        getBigIntEventParam("newDistributorToPoolFlowRate", newDistributorToPoolFlowRate)
    );
    newFlowDistributionUpdatedEvent.parameters.push(
        getBigIntEventParam("newTotalDistributionFlowRate", newTotalDistributionFlowRate)
    );
    newFlowDistributionUpdatedEvent.parameters.push(
        getAddressEventParam("adjustmentFlowRecipient", adjustmentFlowRecipient)
    );
    newFlowDistributionUpdatedEvent.parameters.push(getBigIntEventParam("adjustmentFlowRate", adjustmentFlowRate));
    newFlowDistributionUpdatedEvent.parameters.push(getAddressEventParam("pool", pool));
    newFlowDistributionUpdatedEvent.parameters.push(getAddressEventParam("poolDistributor", poolDistributor));

    return newFlowDistributionUpdatedEvent;
}

export function createDistributionClaimedEvent(
    token: string,
    claimedAmount: BigInt,
    totalClaimed: BigInt,
    pool: string,
    poolMember: string
): DistributionClaimed {
    const newDistributionClaimedEvent = changetype<DistributionClaimed>(newMockEvent());
    newDistributionClaimedEvent.parameters = new Array();
    newDistributionClaimedEvent.parameters.push(getAddressEventParam("token", token));
    newDistributionClaimedEvent.parameters.push(getBigIntEventParam("claimedAmount", claimedAmount));
    newDistributionClaimedEvent.parameters.push(getBigIntEventParam("totalClaimed", totalClaimed));
    newDistributionClaimedEvent.parameters.push(getAddressEventParam("pool", pool));
    newDistributionClaimedEvent.parameters.push(getAddressEventParam("poolMember", poolMember));

    return newDistributionClaimedEvent;
}

export function createMemberUnitsUpdatedEvent(
    token: string,
    units: BigInt,
    pool: string,
    poolMember: string
): MemberUnitsUpdated {
    const newMemberUnitsUpdatedEvent = changetype<MemberUnitsUpdated>(newMockEvent());
    newMemberUnitsUpdatedEvent.parameters = new Array();
    newMemberUnitsUpdatedEvent.parameters.push(getAddressEventParam("token", token));
    newMemberUnitsUpdatedEvent.parameters.push(getBigIntEventParam("units", units));
    newMemberUnitsUpdatedEvent.parameters.push(getAddressEventParam("pool", pool));
    newMemberUnitsUpdatedEvent.parameters.push(getAddressEventParam("poolMember", poolMember));

    return newMemberUnitsUpdatedEvent;
}
