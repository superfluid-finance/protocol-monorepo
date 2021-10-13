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
import { GOERLI_HOST_ADDRESS, GOERLI_RESOLVER_ADDRESS } from "../../utils";
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

let HOST_ADDRESS = Address.fromString(GOERLI_HOST_ADDRESS);
let RESOLVER_ADDRESS = Address.fromString(GOERLI_RESOLVER_ADDRESS);

export function goerliHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleIndexSubscribed(
    event: IndexSubscribedEvent
): void {
    handleIndexSubscribed(event);
}

export function goerliHandleIndexUnitsUpdated(
    event: IndexUnitsUpdatedEvent
): void {
    handleIndexUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleIndexUnsubscribed(
    event: IndexUnsubscribedEvent
): void {
    handleIndexUnsubscribed(event);
}

export function goerliHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}

export function goerliHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS, RESOLVER_ADDRESS);
}
