import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { KOVAN_HOST_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(KOVAN_HOST_ADDRESS);

export function kovanHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS);
}

export function kovanHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS);
}

export function kovanHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS);
}

export function kovanHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS);
}

export function kovanHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS);
}
