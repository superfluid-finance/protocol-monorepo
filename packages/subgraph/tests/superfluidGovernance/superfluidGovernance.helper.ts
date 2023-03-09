import { BigInt, Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    ConfigChanged,
    RewardAddressChanged,
    CFAv1LiquidationPeriodChanged,
    PPPConfigurationChanged,
    TrustedForwarderChanged,
    SuperTokenMinimumDepositChanged,
} from "../../generated/templates/SuperfluidGovernance/SuperfluidGovernanceBase";
import {
    getAddressEventParam,
    getBooleanEventParam,
    getBytesEventParam,
    getBigIntEventParam,
} from "../converters";

export function createConfigChangedEvent(
    host: string,
    superToken: string,
    key: Bytes,
    isKeySet: boolean,
    value: BigInt
): ConfigChanged {
    const newConfigChangedEvent = changetype<ConfigChanged>(newMockEvent());

    newConfigChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const keyParam = getBytesEventParam("key", key);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const valueParam = getBigIntEventParam("value", value);
    newConfigChangedEvent.parameters.push(hostParam);
    newConfigChangedEvent.parameters.push(superTokenParam);
    newConfigChangedEvent.parameters.push(keyParam);
    newConfigChangedEvent.parameters.push(isKeySetParam);
    newConfigChangedEvent.parameters.push(valueParam);

    return newConfigChangedEvent;
}

export function createRewardAddressChangedEvent(
    host: string,
    superToken: string,
    isKeySet: boolean,
    rewardAddress: string
): RewardAddressChanged {
    const newRewardAddressChangedEvent = changetype<RewardAddressChanged>(
        newMockEvent()
    );

    newRewardAddressChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const rewardAddressParam = getAddressEventParam(
        "rewardAddress",
        rewardAddress
    );
    newRewardAddressChangedEvent.parameters.push(hostParam);
    newRewardAddressChangedEvent.parameters.push(superTokenParam);
    newRewardAddressChangedEvent.parameters.push(isKeySetParam);
    newRewardAddressChangedEvent.parameters.push(rewardAddressParam);

    return newRewardAddressChangedEvent;
}

export function createCFAv1LiquidationPeriodChangedEvent(
    host: string,
    superToken: string,
    isKeySet: boolean,
    liquidationPeriod: BigInt
): CFAv1LiquidationPeriodChanged {
    const newCFAv1LiquidationPeriodChangedEvent =
        changetype<CFAv1LiquidationPeriodChanged>(newMockEvent());

    newCFAv1LiquidationPeriodChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const liquidationPeriodParam = getBigIntEventParam(
        "liquidationPeriod",
        liquidationPeriod
    );
    newCFAv1LiquidationPeriodChangedEvent.parameters.push(hostParam);
    newCFAv1LiquidationPeriodChangedEvent.parameters.push(superTokenParam);
    newCFAv1LiquidationPeriodChangedEvent.parameters.push(isKeySetParam);
    newCFAv1LiquidationPeriodChangedEvent.parameters.push(
        liquidationPeriodParam
    );

    return newCFAv1LiquidationPeriodChangedEvent;
}

export function createPPPConfigurationChangedEvent(
    host: string,
    superToken: string,
    isKeySet: boolean,
    liquidationPeriod: BigInt,
    patricianPeriod: BigInt
): PPPConfigurationChanged {
    const newPPPConfigurationChangedEvent = changetype<PPPConfigurationChanged>(
        newMockEvent()
    );

    newPPPConfigurationChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const liquidationPeriodParam = getBigIntEventParam(
        "liquidationPeriod",
        liquidationPeriod
    );
    const patricianPeriodParam = getBigIntEventParam(
        "patricianPeriod",
        patricianPeriod
    );
    newPPPConfigurationChangedEvent.parameters.push(hostParam);
    newPPPConfigurationChangedEvent.parameters.push(superTokenParam);
    newPPPConfigurationChangedEvent.parameters.push(isKeySetParam);
    newPPPConfigurationChangedEvent.parameters.push(liquidationPeriodParam);
    newPPPConfigurationChangedEvent.parameters.push(patricianPeriodParam);

    return newPPPConfigurationChangedEvent;
}

export function createTrustedForwarderChangedEvent(
    host: string,
    superToken: string,
    isKeySet: boolean,
    forwarder: string,
    enabled: boolean
): TrustedForwarderChanged {
    const newTrustedForwarderChangedEvent = changetype<TrustedForwarderChanged>(
        newMockEvent()
    );

    newTrustedForwarderChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const forwarderParam = getAddressEventParam("forwarder", forwarder);
    const enabledParam = getBooleanEventParam("enabled", enabled);
    newTrustedForwarderChangedEvent.parameters.push(hostParam);
    newTrustedForwarderChangedEvent.parameters.push(superTokenParam);
    newTrustedForwarderChangedEvent.parameters.push(isKeySetParam);
    newTrustedForwarderChangedEvent.parameters.push(forwarderParam);
    newTrustedForwarderChangedEvent.parameters.push(enabledParam);

    return newTrustedForwarderChangedEvent;
}

export function createSuperTokenMinimumDepositChangedEvent(
    host: string,
    superToken: string,
    isKeySet: boolean,
    minimumDeposit: BigInt
): SuperTokenMinimumDepositChanged {
    const newSuperTokenMinimumDepositChangedEvent =
        changetype<SuperTokenMinimumDepositChanged>(newMockEvent());

    newSuperTokenMinimumDepositChangedEvent.parameters = new Array();
    const hostParam = getAddressEventParam("host", host);
    const superTokenParam = getAddressEventParam("superToken", superToken);
    const isKeySetParam = getBooleanEventParam("isKeySet", isKeySet);
    const minimumDepositParam = getBigIntEventParam(
        "minimumDeposit",
        minimumDeposit
    );
    newSuperTokenMinimumDepositChangedEvent.parameters.push(hostParam);
    newSuperTokenMinimumDepositChangedEvent.parameters.push(superTokenParam);
    newSuperTokenMinimumDepositChangedEvent.parameters.push(isKeySetParam);
    newSuperTokenMinimumDepositChangedEvent.parameters.push(
        minimumDepositParam
    );

    return newSuperTokenMinimumDepositChangedEvent;
}
