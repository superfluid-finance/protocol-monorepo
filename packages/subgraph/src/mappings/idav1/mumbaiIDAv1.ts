import { Address } from "@graphprotocol/graph-ts";
import {
    IndexCreated as IndexCreatedEvent,
    IndexUpdated as IndexUpdatedEvent,
    SubscriptionApproved as SubscriptionApprovedEvent,
    SubscriptionRevoked as SubscriptionRevokedEvent,
    SubscriptionUnitsUpdated as SubscriptionUnitsUpdatedEvent,
} from "../../../generated/InstantDistributionAgreementV1/IInstantDistributionAgreementV1";
import { MUMBAI_HOST_ADDRESS } from "../../utils";
import {
    handleIndexCreated,
    handleIndexUpdated,
    handleSubscriptionApproved,
    handleSubscriptionRevoked,
    handleSubscriptionUnitsUpdated,
} from "./idav1Base";

let HOST_ADDRESS = Address.fromString(MUMBAI_HOST_ADDRESS);

export function mumbaiHandleIndexCreated(event: IndexCreatedEvent): void {
    handleIndexCreated(event, HOST_ADDRESS);
}

export function mumbaiHandleIndexUpdated(event: IndexUpdatedEvent): void {
    handleIndexUpdated(event, HOST_ADDRESS);
}

export function mumbaiHandleSubscriptionApproved(
    event: SubscriptionApprovedEvent
): void {
    handleSubscriptionApproved(event, HOST_ADDRESS);
}

export function mumbaiHandleSubscriptionRevoked(
    event: SubscriptionRevokedEvent
): void {
    handleSubscriptionRevoked(event, HOST_ADDRESS);
}

export function mumbaiHandleSubscriptionUnitsUpdated(
    event: SubscriptionUnitsUpdatedEvent
): void {
    handleSubscriptionUnitsUpdated(event, HOST_ADDRESS);
}
