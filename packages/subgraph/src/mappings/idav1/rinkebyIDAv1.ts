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
import { RINKEBY_HOST_ADDRESS, RINKEBY_RESOLVER_ADDRESS } from "../../utils";
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

let HOST_ADDRESS = Address.fromString(RINKEBY_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(RINKEBY_RESOLVER_ADDRESS);

export function rinkebyHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleIndexSubscribed(
    event: IndexSubscribedEvent
): void {
    handleIndexSubscribed(event);
}

export function rinkebyHandleIndexUnitsUpdated(
    event: IndexUnitsUpdatedEvent
): void {
    handleIndexUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleIndexUnsubscribed(
    event: IndexUnsubscribedEvent
): void {
    handleIndexUnsubscribed(event);
}

export function rinkebyHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function rinkebyHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}
