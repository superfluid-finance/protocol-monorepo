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
import { XDAI_HOST_ADDRESS, XDAI_RESOLVER_ADDRESS } from "../../utils";
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

let HOST_ADDRESS = Address.fromString(XDAI_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(XDAI_RESOLVER_ADDRESS);

export function xdaiHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleIndexSubscribed(
    event: IndexSubscribedEvent
): void {
    handleIndexSubscribed(event);
}

export function xdaiHandleIndexUnitsUpdated(
    event: IndexUnitsUpdatedEvent
): void {
    handleIndexUnitsUpdated(event);
}

export function xdaiHandleIndexUnsubscribed(
    event: IndexUnsubscribedEvent
): void {
    handleIndexUnsubscribed(event);
}

export function xdaiHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function xdaiHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}
