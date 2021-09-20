import { Address } from "@graphprotocol/graph-ts";
import {
    CustomSuperTokenCreated as CustomSuperTokenCreatedEvent,
    SuperTokenCreated as SuperTokenCreatedEvent,
    SuperTokenLogicCreated as SuperTokenLogicCreatedEvent,
} from "../../../generated/SuperTokenFactory/ISuperTokenFactory";
import { RINKEBY_HOST_ADDRESS } from "../../utils";
import {
    handleCustomSuperTokenCreated,
    handleSuperTokenCreated,
    handleSuperTokenLogicCreated,
} from "./superTokenFactoryBase";

let HOST_ADDRESS = Address.fromString(RINKEBY_HOST_ADDRESS);

export function rinkebyHandleSuperTokenCreated(
    event: SuperTokenCreatedEvent
): void {
    handleSuperTokenCreated(event, HOST_ADDRESS);
}

export function rinkebyHandleCustomSuperTokenCreated(
    event: CustomSuperTokenCreatedEvent
): void {
    handleCustomSuperTokenCreated(event, HOST_ADDRESS);
}

export function rinkebyHandleSuperTokenLogicCreated(
    event: SuperTokenLogicCreatedEvent
): void {
    handleSuperTokenLogicCreated(event, HOST_ADDRESS);
}
