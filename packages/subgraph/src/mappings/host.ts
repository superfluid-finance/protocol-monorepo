import {
    AgreementClassRegisteredEvent,
    AgreementClassUpdatedEvent,
    AppRegisteredEvent,
    GovernanceReplacedEvent,
    JailEvent,
    SFMeta,
    SuperTokenFactoryUpdatedEvent,
    SuperTokenLogicUpdatedEvent,
} from "../../generated/schema";
import {
    AgreementClassRegistered,
    AgreementClassUpdated,
    AppRegistered,
    GovernanceReplaced,
    Jail,
    SuperTokenFactoryUpdated,
    SuperTokenLogicUpdated,
} from "../../generated/Host/ISuperfluid";
import { createEventID, initializeEventEntity } from "../utils";
import { commitHash, configuration, branch } from "../meta.ignore";
import { ethereum } from "@graphprotocol/graph-ts";
import { SuperfluidGovernance } from "../../generated/templates";

export function handleGovernanceReplaced(event: GovernanceReplaced): void {
    const eventId = createEventID("GovernanceReplaced", event);
    let ev = new GovernanceReplacedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "GovernanceReplaced",
        event,
        []
    ) as GovernanceReplacedEvent;
    ev.oldGovernance = event.params.oldGov;
    ev.newGovernance = event.params.newGov;
    ev.logIndex = event.logIndex;
    ev.save();

    // Create data source template for new Governance contract
    // and start indexing events
    // @note The subgraph will not capture governance events
    // which occur prior to this event being emitted
    SuperfluidGovernance.create(event.params.newGov);
}

export function handleAgreementClassRegistered(
    event: AgreementClassRegistered
): void {
    const eventId = createEventID("AgreementClassRegistered", event);
    let ev = new AgreementClassRegisteredEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "AgreementClassRegistered",
        event,
        []
    ) as AgreementClassRegisteredEvent;
    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();

    initSFMetaOnce(event);
}

export function handleAgreementClassUpdated(
    event: AgreementClassUpdated
): void {
    const eventId = createEventID("AgreementClassUpdated", event);
    let ev = new AgreementClassUpdatedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "AgreementClassUpdated",
        event,
        []
    ) as AgreementClassUpdatedEvent;

    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();

    // NOTE: It appears there are no AgreementClassRegisteredEvents on Goerli
    initSFMetaOnce(event);
}

export function handleSuperTokenFactoryUpdated(
    event: SuperTokenFactoryUpdated
): void {
    const eventId = createEventID("SuperTokenFactoryUpdated", event);
    let ev = new SuperTokenFactoryUpdatedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "SuperTokenFactoryUpdated",
        event,
        []
    ) as SuperTokenFactoryUpdatedEvent;

    ev.newFactory = event.params.newFactory;
    ev.save();
}

export function handleSuperTokenLogicUpdated(
    event: SuperTokenLogicUpdated
): void {
    const eventId = createEventID("SuperTokenLogicUpdated", event);
    let ev = new SuperTokenLogicUpdatedEvent(eventId);
    ev = initializeEventEntity(
        ev,
        "SuperTokenLogicUpdated",
        event,
        []
    ) as SuperTokenLogicUpdatedEvent;

    ev.token = event.params.token;
    ev.code = event.params.code;
    ev.save();
}

export function handleAppRegistered(event: AppRegistered): void {
    let ev = new AppRegisteredEvent(createEventID("AppRegistered", event));
    ev = initializeEventEntity(
        ev,
        "AppRegistered",
        event,
        []
    ) as AppRegisteredEvent;

    ev.app = event.params.app;
    ev.save();
}

export function handleJail(event: Jail): void {
    let ev = new JailEvent(createEventID("Jail", event));
    ev = initializeEventEntity(ev, "Jail", event, []) as JailEvent;

    ev.app = event.params.app;
    ev.reason = event.params.reason;
    ev.save();
}

function initSFMetaOnce(event: ethereum.Event): void {
    let sfMeta = SFMeta.load(commitHash);
    if (sfMeta == null) {
        sfMeta = new SFMeta(commitHash);
        sfMeta.timestamp = event.block.timestamp;
        sfMeta.blockNumber = event.block.number;
        sfMeta.configuration = configuration;
        sfMeta.branch = branch;
        sfMeta.save();
    }
}
