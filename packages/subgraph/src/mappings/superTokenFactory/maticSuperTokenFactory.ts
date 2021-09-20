import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import { MATIC_HOST_ADDRESS } from "../../utils";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "./superTokenFactoryBase";

let HOST_ADDRESS = Address.fromString(MATIC_HOST_ADDRESS);

export function maticHandleSuperTokenCreated(
    event: SuperTokenCreatedEvent
): void {
    handleSuperTokenCreated(event, HOST_ADDRESS);
}

export function maticHandleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    handleCustomSuperTokenCreated(event, HOST_ADDRESS);
}

export function maticHandleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    handleSuperTokenLogicCreated(event, HOST_ADDRESS);
}
