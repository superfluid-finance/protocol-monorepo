import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexSubscribed as IndexSubscribedEvent,
    IndexUnitsUpdated as IndexUnitsUpdatedEvent,
    IndexUnsubscribed as IndexUnsubscribedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { KOVAN_HOST_ADDRESS, KOVAN_RESOLVER_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexSubscribed,
    handleIndexUnitsUpdated,
    handleIndexUnsubscribed,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(KOVAN_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(KOVAN_RESOLVER_ADDRESS);

export function kovanHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleIndexSubscribed(
    event: IndexSubscribedEvent
): void {
    handleIndexSubscribed(event);
}

export function kovanHandleIndexUnitsUpdated(
    event: IndexUnitsUpdatedEvent
): void {
    handleIndexUnitsUpdated(event);
}

export function kovanHandleIndexUnsubscribed(
    event: IndexUnsubscribedEvent
): void {
    handleIndexUnsubscribed(event);
}

export function kovanHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function kovanHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}
