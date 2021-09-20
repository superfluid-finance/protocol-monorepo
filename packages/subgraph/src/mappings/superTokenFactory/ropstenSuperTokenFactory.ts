import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import { ROPSTEN_HOST_ADDRESS } from "../../utils";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "./superTokenFactoryBase";

let HOST_ADDRESS = Address.fromString(ROPSTEN_HOST_ADDRESS);

export function ropstenHandleSuperTokenCreated(
    event: SuperTokenCreatedEvent
): void {
    handleSuperTokenCreated(event, HOST_ADDRESS);
}

export function ropstenHandleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    handleCustomSuperTokenCreated(event, HOST_ADDRESS);
}

export function ropstenHandleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    handleSuperTokenLogicCreated(event, HOST_ADDRESS);
}
