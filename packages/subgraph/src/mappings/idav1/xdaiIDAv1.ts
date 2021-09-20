import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { XDAI_HOST_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(XDAI_HOST_ADDRESS);

export function xdaiHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS);
}

export function xdaiHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS);
}

export function xdaiHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS);
}

export function xdaiHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS);
}

export function xdaiHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS);
}
