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
import { BIG_INT_ZERO, createEventID, getOrder } from "../utils";
import { commitHash, configuration, branch } from "../meta.ignore";
import { ethereum } from "@graphprotocol/graph-ts";
import { SuperfluidGovernance } from "../../generated/templates";

export function handleGovernanceReplaced(event: GovernanceReplaced): void {
    let ev = new GovernanceReplacedEvent(
        createEventID("GovernanceReplaced", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "GovernanceReplaced";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.order = getOrder(event.block.number, event.logIndex);
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
    let ev = new AgreementClassRegisteredEvent(
        createEventID("AgreementClassRegistered", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "AgreementClassRegistered";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();

    initSFMetaOnce(event);
}

export function handleAgreementClassUpdated(
    event: AgreementClassUpdated
): void {
    let ev = new AgreementClassUpdatedEvent(
        createEventID("AgreementClassUpdated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "AgreementClassUpdated";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.agreementType = event.params.agreementType;
    ev.code = event.params.code;
    ev.save();

    // NOTE: It appears there are no AgreementClassRegisteredEvents on Goerli
    initSFMetaOnce(event);
}

export function handleSuperTokenFactoryUpdated(
    event: SuperTokenFactoryUpdated
): void {
    let ev = new SuperTokenFactoryUpdatedEvent(
        createEventID("SuperTokenFactoryUpdated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "SuperTokenFactoryUpdated";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.newFactory = event.params.newFactory;
    ev.save();
}

export function handleSuperTokenLogicUpdated(
    event: SuperTokenLogicUpdated
): void {
    let ev = new SuperTokenLogicUpdatedEvent(
        createEventID("SuperTokenLogicUpdated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "SuperTokenLogicUpdated";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.params.token;
    ev.code = event.params.code;
    ev.save();
}

export function handleAppRegistered(event: AppRegistered): void {
    let ev = new AppRegisteredEvent(createEventID("AppRegistered", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "AppRegistered";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.app = event.params.app;
    ev.save();
}

export function handleJail(event: Jail): void {
    let ev = new JailEvent(createEventID("Jail", event));
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "Jail";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
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
