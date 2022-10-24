import { Bytes } from "@graphprotocol/graph-ts";
import { newMockEvent } from "matchstick-as";
import { Set } from "../../generated/ResolverV1/Resolver";
import { getAddressEventParam, getBytesEventParam } from "../converters";

export function createSetEvent(name: Bytes, target: string): Set {
    const newSetEvent = changetype<Set>(newMockEvent());

    newSetEvent.parameters = new Array();
    const nameParam = getBytesEventParam("name", name);
    const targetParam = getAddressEventParam("target", target);
    newSetEvent.parameters.push(nameParam);
    newSetEvent.parameters.push(targetParam);

    return newSetEvent;
}
