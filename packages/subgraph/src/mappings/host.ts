import {
    AgreementClassRegistered,
    AgreementClassUpdated,
    AppRegistered,
    GovernanceReplaced,
    Jail,
    SuperTokenFactoryUpdated,
    SuperTokenLogicUpdated,
} from "../../generated/schema";
import {
    AgreementClassRegistered as AgreementClassRegisteredEvent,
    AgreementClassUpdated as AgreementClassUpdatedEvent,
    AppRegistered as AppRegisteredEvent,
    GovernanceReplaced as GovernanceReplacedEvent,
    Jail as JailEvent,
    SuperTokenFactoryUpdated as SuperTokenFactoryUpdatedEvent,
    SuperTokenLogicUpdated as SuperTokenLogicUpdatedEvent,
} from "../../generated/Host/ISuperfluid";
import { createEventID } from "../utils";

export function handleGovernanceReplaced(event: GovernanceReplacedEvent): void {
    let ev = new GovernanceReplaced(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.oldGovernance = event.params.oldGov;
    ev.newGovernance = event.params.newGov;
    ev.save();
}

export function handleAgreementClassRegistered(
    event: AgreementClassRegisteredEvent
): void {
    let ev = new AgreementClassRegistered(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();
}

export function handleAgreementClassUpdated(
    event: AgreementClassUpdatedEvent
): void {
    let ev = new AgreementClassUpdated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();
}

export function handleSuperTokenFactoryUpdated(
    event: SuperTokenFactoryUpdatedEvent
): void {
    let ev = new SuperTokenFactoryUpdated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.newFactory = event.params.newFactory;
    ev.save();
}

export function handleSuperTokenLogicUpdated(
    event: SuperTokenLogicUpdatedEvent
): void {
    let ev = new SuperTokenLogicUpdated(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.token = event.params.token;
    ev.code = event.params.code;
    ev.save();
}

export function handleAppRegistered(event: AppRegisteredEvent): void {
    let ev = new AppRegistered(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.app = event.params.app;
    ev.save();
}

export function handleJail(event: JailEvent): void {
    let ev = new Jail(createEventID(event));
    ev.transactionHash = event.transaction.hash;
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.app = event.params.app;
    ev.reason = event.params.reason;
    ev.save();
}
