import {
    Address,
    BigInt,
    ByteArray,
    Bytes,
    crypto,
    ethereum,
} from "@graphprotocol/graph-ts";

/**
 * Takes a string and returns the keccak256 hash of it as Bytes type.
 * @param value string parameter value
 * @returns Bytes
 */
export function keccak256String(value: string): Bytes {
    const byteArrayString = ByteArray.fromUTF8(value);
    return Bytes.fromByteArray(crypto.keccak256(byteArrayString));
}

/**
 * Takes a string and returns it as Bytes type.
 * @param value string parameter value
 * @returns Bytes
 */
export function stringToBytes(value: string): Bytes {
    const byteArrayString = ByteArray.fromUTF8(value);
    return Bytes.fromByteArray(byteArrayString);
}

/**
 * Takes a string param and returns a string ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value string parameter value
 * @returns ethereum.EventParam
 */
export function getStringEventParam(
    name: string,
    value: string
): ethereum.EventParam {
    return new ethereum.EventParam(name, ethereum.Value.fromString(value));
}

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
 * Takes a BigInt value and returns a BigInt ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value BigInt parameter value
 * @returns ethereum.EventParam
 */
export function getBigIntEventParam(
    name: string,
    value: BigInt
): ethereum.EventParam {
    return new ethereum.EventParam(
        name,
        ethereum.Value.fromUnsignedBigInt(value)
    );
}

/**
 * Takes an i32 value and returns an i32 ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value i32 parameter value
 * @returns ethereum.EventParam
 */
export function getI32EventParam(
    name: string,
    value: i32
): ethereum.EventParam {
    return new ethereum.EventParam(name, ethereum.Value.fromI32(value));
}

/**
 * Takes a boolean value and returns a boolean ethereum.EventParam object
 * @param name the name of the parameter (must match actual value from contracts)
 * @param value boolean parameter value
 * @returns ethereum.EventParam
 */
export function getBooleanEventParam(
    name: string,
    value: boolean
): ethereum.EventParam {
    return new ethereum.EventParam(name, ethereum.Value.fromBoolean(value));
}
