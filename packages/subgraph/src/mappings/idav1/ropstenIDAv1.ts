import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { ROPSTEN_HOST_ADDRESS, ROPSTEN_RESOLVER_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(ROPSTEN_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(ROPSTEN_RESOLVER_ADDRESS);

export function ropstenHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function ropstenHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}
