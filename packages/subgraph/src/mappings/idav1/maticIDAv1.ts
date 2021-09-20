import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { MATIC_HOST_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(MATIC_HOST_ADDRESS);

export function maticHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS);
}

export function maticHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS);
}

export function maticHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS);
}

export function maticHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS);
}

export function maticHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS);
}
