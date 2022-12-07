import { BigInt, Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    AgreementClassRegistered,
    AppRegistered,
    GovernanceReplaced,
    Jail,
    SuperTokenFactoryUpdated,
    SuperTokenLogicUpdated,
} from "../../generated/Host/ISuperfluid";
import {
    getAddressEventParam,
    getBytesEventParam,
    getBigIntEventParam,
} from "../converters";

export function createGovernanceReplacedEvent(
    oldGovernance: string,
    newGovernance: string
): GovernanceReplaced {
    const newGovernanceReplacedEvent = changetype<GovernanceReplaced>(
        newMockEvent()
    );

    newGovernanceReplacedEvent.parameters = new Array();
    const oldGov = getAddressEventParam("oldGov", oldGovernance);
    const newGov = getAddressEventParam("newGov", newGovernance);
    newGovernanceReplacedEvent.parameters.push(oldGov);
    newGovernanceReplacedEvent.parameters.push(newGov);

    return newGovernanceReplacedEvent;
}

export function createAgreementClassRegisteredEvent(
    agreementType: Bytes,
    code: string
): AgreementClassRegistered {
    const newAgreementClassRegisteredEvent =
        changetype<AgreementClassRegistered>(newMockEvent());

    newAgreementClassRegisteredEvent.parameters = new Array();
    const agreementTypeParam = getBytesEventParam(
        "agreementType",
        agreementType
    );
    const codeParam = getAddressEventParam("code", code);
    newAgreementClassRegisteredEvent.parameters.push(agreementTypeParam);
    newAgreementClassRegisteredEvent.parameters.push(codeParam);

    return newAgreementClassRegisteredEvent;
}

export function createSuperTokenFactoryUpdatedEvent(
    newFactory: string
): SuperTokenFactoryUpdated {
    const newSuperTokenFactoryUpdatedEvent =
        changetype<SuperTokenFactoryUpdated>(newMockEvent());

    newSuperTokenFactoryUpdatedEvent.parameters = new Array();
    const newFactoryParam = getAddressEventParam("newFactory", newFactory);
    newSuperTokenFactoryUpdatedEvent.parameters.push(newFactoryParam);

    return newSuperTokenFactoryUpdatedEvent;
}

export function createSuperTokenLogicUpdatedEvent(
    token: string,
    code: string
): SuperTokenLogicUpdated {
    const newSuperTokenLogicUpdatedEvent = changetype<SuperTokenLogicUpdated>(
        newMockEvent()
    );

    newSuperTokenLogicUpdatedEvent.parameters = new Array();

    const tokenParam = getAddressEventParam("token", token);
    const codeParam = getAddressEventParam("code", code);
    newSuperTokenLogicUpdatedEvent.parameters.push(tokenParam);
    newSuperTokenLogicUpdatedEvent.parameters.push(codeParam);

    return newSuperTokenLogicUpdatedEvent;
}

export function createAppRegisteredEvent(app: string): AppRegistered {
    const newAppRegisteredEvent = changetype<AppRegistered>(newMockEvent());

    newAppRegisteredEvent.parameters = new Array();
    const appParam = getAddressEventParam("app", app);
    newAppRegisteredEvent.parameters.push(appParam);

    return newAppRegisteredEvent;
}

export function createJailEvent(app: string, reason: BigInt): Jail {
    const newJailEvent = changetype<Jail>(newMockEvent());

    newJailEvent.parameters = new Array();
    const appParam = getAddressEventParam("app", app);
    const reasonParam = getBigIntEventParam("reason", reason);
    newJailEvent.parameters.push(appParam);
    newJailEvent.parameters.push(reasonParam);

    return newJailEvent;
}
