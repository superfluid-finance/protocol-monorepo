import {
    CustomSuperTokenCreated,
    SuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import {
    CustomSuperTokenCreatedEvent,
    SuperTokenCreatedEvent,
    SuperTokenLogicCreatedEvent,
} from "../../generated/schema";
import {
    BIG_INT_ZERO,
    createEventID,
    getOrder,
    tokenHasValidHost,
} from "../utils";
import { getOrInitSuperToken } from "../mappingHelpers";
import { getHostAddress } from "../addresses";

export function handleSuperTokenCreated(event: SuperTokenCreated): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenCreatedEvent(
        createEventID("SuperTokenCreated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "SuperTokenCreated";
    ev.addresses = [event.params.token];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, event.block);
}

export function handleCustomSuperTokenCreated(
    event: CustomSuperTokenCreated
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.token);
    if (!hasValidHost) {
        return;
    }

    let ev = new CustomSuperTokenCreatedEvent(
        createEventID("CustomSuperTokenCreated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "CustomSuperTokenCreated";
    ev.addresses = [event.params.token];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.params.token;
    ev.save();

    getOrInitSuperToken(event.params.token, event.block);
}

export function handleSuperTokenLogicCreated(
    event: SuperTokenLogicCreated
): void {
    let hostAddress = getHostAddress();
    let hasValidHost = tokenHasValidHost(hostAddress, event.params.tokenLogic);
    if (!hasValidHost) {
        return;
    }

    let ev = new SuperTokenLogicCreatedEvent(
        createEventID("SuperTokenLogicCreated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.gasPrice = event.transaction.gasPrice;
    if (event.receipt) {
        ev.gasUsed = event.receipt.gasUsed;
    } else {
        ev.gasUsed = BIG_INT_ZERO;
    }
    ev.timestamp = event.block.timestamp;
    ev.name = "SuperTokenLogicCreated";
    ev.addresses = [];
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.tokenLogic = event.params.tokenLogic;
    ev.save();
}
