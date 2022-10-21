import { Address, BigInt, Bytes, ethereum } from "@graphprotocol/graph-ts";

/**
 * Takes a string address and returns an Address ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value string parameter value
 * @returns ethereum.EventParam
 */
export function getAddressEventParam(
    name: string,
    value: string
): ethereum.EventParam {
    return new ethereum.EventParam(
        name,
        ethereum.Value.fromAddress(Address.fromString(value))
    );
}

/**
 * Takes a bytes value and returns a Bytes ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value Bytes parameter value
 * @returns ethereum.EventParam
 */
export function getBytesEventParam(
    name: string,
    value: Bytes
): ethereum.EventParam {
    return new ethereum.EventParam(name, ethereum.Value.fromBytes(value));
}

/**
 * Takes a bytes value and returns a Bytes ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value BigInt parameter value
 * @returns ethereum.EventParam
 */
export function getUintEventParam(
    name: string,
    value: BigInt
): ethereum.EventParam {
    return new ethereum.EventParam(
        name,
        ethereum.Value.fromUnsignedBigInt(value)
    );
}
