import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import { MUMBAI_HOST_ADDRESS } from "../../utils";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "./superTokenFactoryBase";

let HOST_ADDRESS = Address.fromString(MUMBAI_HOST_ADDRESS);

export function mumbaiHandleSuperTokenCreated(
    event: SuperTokenCreatedEvent
): void {
    handleSuperTokenCreated(event, HOST_ADDRESS);
}

export function mumbaiHandleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    handleCustomSuperTokenCreated(event, HOST_ADDRESS);
}

export function mumbaiHandleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    handleSuperTokenLogicCreated(event, HOST_ADDRESS);
}
