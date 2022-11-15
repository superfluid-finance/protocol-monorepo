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
    const ev = new ConfigChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.key = event.params.key;
    ev.isKeySet = event.params.isKeySet;
    ev.value = event.params.value;
    ev.save();
}

export function handleRewardAddressChanged(event: RewardAddressChanged): void {
    const eventId = createEventID("RewardAddressChanged", event);
    const ev = new RewardAddressChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
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
    const ev = new CFAv1LiquidationPeriodChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
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
    const ev = new PPPConfigurationChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
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
    const ev = new TrustedForwarderChangedEvent(eventId);
    initializeEventEntity(ev, event, []);

    ev.governanceAddress = event.address;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isKeySet = event.params.isKeySet;
    ev.forwarder = event.params.forwarder;
    ev.enabled = event.params.enabled;
    ev.save();
}
