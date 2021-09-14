import {
    ConfigChanged as ConfigChangedEvent,
    RewardAddressChanged as RewardAddressChangedEvent,
    CFAv1LiquidationPeriodChanged as CFAv1LiquidationPeriodChangedEvent,
    TrustedForwarderChanged as TrustedForwarderChangedEvent,
} from "../../generated/SuperfluidGovernance/SuperfluidGovernanceBase";
import {
    CFAv1LiquidationPeriodChanged,
    ConfigChanged,
    RewardAddressChanged,
    TrustedForwarderChanged,
} from "../../generated/schema";
import { createEventID } from "../utils";

export function handleConfigChanged(event: ConfigChangedEvent): void {
    let ev = new ConfigChanged(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.key = event.params.key;
    ev.isSet = event.params.set;
    ev.value = event.params.value;
    ev.save();
}

export function handleRewardAddressChanged(
    event: RewardAddressChangedEvent
): void {
    let ev = new RewardAddressChanged(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isSet = event.params.set;
    ev.rewardAddress = event.params.rewardAddress;
    ev.save();
}

export function handleCFAv1LiquidationPeriodChanged(
    event: CFAv1LiquidationPeriodChangedEvent
): void {
    let ev = new CFAv1LiquidationPeriodChanged(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isSet = event.params.set;
    ev.liquidationPeriod = event.params.liquidationPeriod;
    ev.save();
}

export function handleTrustedForwarderChanged(
    event: TrustedForwarderChangedEvent
): void {
    let ev = new TrustedForwarderChanged(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.host = event.params.host;
    ev.superToken = event.params.superToken;
    ev.isSet = event.params.set;
    ev.forwarder = event.params.forwarder;
    ev.enabled = event.params.enabled;
    ev.save();
}
