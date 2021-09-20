import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import { KOVAN_HOST_ADDRESS } from "../../utils";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "./superTokenFactoryBase";

let HOST_ADDRESS = Address.fromString(KOVAN_HOST_ADDRESS);

export function kovanHandleSuperTokenCreated(
    event: SuperTokenCreatedEvent
): void {
    handleSuperTokenCreated(event, HOST_ADDRESS);
}

export function kovanHandleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    handleCustomSuperTokenCreated(event, HOST_ADDRESS);
}

export function kovanHandleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    handleSuperTokenLogicCreated(event, HOST_ADDRESS);
}
