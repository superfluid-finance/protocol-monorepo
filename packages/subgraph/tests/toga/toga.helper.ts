import { BigInt } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import {
    BondIncreased,
    ExitRateChanged,
    NewPIC,
} from "../../generated/templates/TOGA/TOGA";
import { getAddressEventParam, getBigIntEventParam } from "../converters";

export function createBondIncreasedEvent(
    token: string,
    additionalBond: BigInt
): BondIncreased {
    const bondIncreasedEvent = changetype<BondIncreased>(newMockEvent());

    const tokenParam = getAddressEventParam("token", token);
    const additionalBondParam = getBigIntEventParam(
        "additionalBond",
        additionalBond
    );
    bondIncreasedEvent.parameters = new Array();
    bondIncreasedEvent.parameters.push(tokenParam);
    bondIncreasedEvent.parameters.push(additionalBondParam);

    return bondIncreasedEvent;
}

export function createExitRateChangedEvent(
    token: string,
    exitRate: BigInt
): ExitRateChanged {
    const exitRateChangedEvent = changetype<ExitRateChanged>(newMockEvent());

    const tokenParam = getAddressEventParam("token", token);
    const exitRateParam = getBigIntEventParam("exitRate", exitRate);

    exitRateChangedEvent.parameters = new Array();
    exitRateChangedEvent.parameters.push(tokenParam);
    exitRateChangedEvent.parameters.push(exitRateParam);

    return exitRateChangedEvent;
}

export function createNewPICEvent(
    token: string,
    pic: string,
    bond: BigInt,
    exitRate: BigInt
): NewPIC {
    const newPICEvent = changetype<NewPIC>(newMockEvent());
    const tokenParam = getAddressEventParam("token", token);
    const picParam = getAddressEventParam("pic", pic);
    const bondParam = getBigIntEventParam("bond", bond);
    const exitRateParam = getBigIntEventParam("exitRate", exitRate);

    newPICEvent.parameters = new Array();
    newPICEvent.parameters.push(tokenParam);
    newPICEvent.parameters.push(picParam);
    newPICEvent.parameters.push(bondParam);
    newPICEvent.parameters.push(exitRateParam);

    return newPICEvent;
}
