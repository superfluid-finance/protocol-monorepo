import { newMockEvent } from "matchstick-as";
import {
    SuperTokenCreated,
    CustomSuperTokenCreated,
    SuperTokenLogicCreated,
} from "../../generated/SuperTokenFactory/ISuperTokenFactory";
import { getAddressEventParam } from "../converters";

export function createSuperTokenCreatedEvent(token: string): SuperTokenCreated {
    const newSuperTokenCreatedEvent = changetype<SuperTokenCreated>(
        newMockEvent()
    );

    newSuperTokenCreatedEvent.parameters = new Array();
    const tokenParam = getAddressEventParam("token", token);
    newSuperTokenCreatedEvent.parameters.push(tokenParam);

    return newSuperTokenCreatedEvent;
}

export function createCustomSuperTokenCreatedEvent(
    token: string
): CustomSuperTokenCreated {
    const newCustomSuperTokenCreatedEvent = changetype<CustomSuperTokenCreated>(
        newMockEvent()
    );

    newCustomSuperTokenCreatedEvent.parameters = new Array();
    const tokenParam = getAddressEventParam("token", token);
    newCustomSuperTokenCreatedEvent.parameters.push(tokenParam);

    return newCustomSuperTokenCreatedEvent;
}

export function createSuperTokenLogicCreatedEvent(
    tokenLogic: string
): SuperTokenLogicCreated {
    const newSuperTokenLogicCreatedEvent = changetype<SuperTokenLogicCreated>(
        newMockEvent()
    );

    newSuperTokenLogicCreatedEvent.parameters = new Array();
    const tokenLogicParam = getAddressEventParam("tokenLogic", tokenLogic);
    newSuperTokenLogicCreatedEvent.parameters.push(tokenLogicParam);

    return newSuperTokenLogicCreatedEvent;
}