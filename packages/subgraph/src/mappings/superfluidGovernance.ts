import {
    ConfigChanged,
    RewardAddressChanged,
    CFAv1LiquidationPeriodChanged,
    TrustedForwarderChanged,
    PPPConfigurationChanged,
} from "../../generated/templates/SuperfluidGovernance/SuperfluidGovernanceBase";
import {
    CFAv1LiquidationPeriodChangedEvent,
    ConfigChangedEvent,
    RewardAddressChangedEvent,
    PPPConfigurationChangedEvent,
    TrustedForwarderChangedEvent,
} from "../../generated/schema";
import { createEventID, initializeEventEntity } from "../utils";

export function handleConfigChanged(event: ConfigChanged): void {
    const eventId = createEventID("ConfigChanged", event);
    let ev = new ConfigChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "ConfigChanged",
        event,
        []
    ) as ConfigChangedEvent;

    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.key = event.params.key;
    ev.isKeySet = event.params.isKeySet;
    ev.value = event.params.value;
    ev.save();
}

export function handleRewardAddressChanged(event: RewardAddressChanged): void {
    const eventId = createEventID("RewardAddressChanged", event);
    let ev = new RewardAddressChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "RewardAddressChanged",
        event,
        []
    ) as RewardAddressChangedEvent;

    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.rewardAddress = event.params.rewardAddress;
    ev.save();
}

export function handleCFAv1LiquidationPeriodChanged(
    event: CFAv1LiquidationPeriodChanged
): void {
    const eventId = createEventID("CFAv1LiquidationPeriodChanged", event);
    let ev = new CFAv1LiquidationPeriodChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "CFAv1LiquidationPeriodChanged",
        event,
        []
    ) as CFAv1LiquidationPeriodChangedEvent;

    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.liquidationPeriod = event.params.liquidationPeriod;
    ev.save();
}

export function handlePPPConfigurationChanged(
    event: PPPConfigurationChanged
): void {
    const eventId = createEventID("PPPConfigurationChanged", event);
    let ev = new PPPConfigurationChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "PPPConfigurationChanged",
        event,
        []
    ) as PPPConfigurationChangedEvent;

    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.liquidationPeriod = event.params.liquidationPeriod;
    ev.patricianPeriod = event.params.patricianPeriod;
    ev.save();
}

export function handleTrustedForwarderChanged(
    event: TrustedForwarderChanged
): void {
    const eventId = createEventID("TrustedForwarderChanged", event);
    let ev = new TrustedForwarderChangedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "TrustedForwarderChanged",
        event,
        []
    ) as TrustedForwarderChangedEvent;

    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.forwarder = event.params.forwarder;
    ev.enabled = event.params.enabled;
    ev.save();
}
