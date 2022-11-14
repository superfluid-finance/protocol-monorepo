import { Address, TypedMap } from "@graphprotocol/graph-ts";

// This file is a template file which is used for getting the address
// based on the network we set in the set-network package.json file.
// We add a bit of complexity to the package.json, but remove much
// more as a result.
// @note TODO: I am pretty sure we can remove {{network}} and just do
// return Address.fromString("{{hostAddress}}")
export function getHostAddress(): Address {
    return Address.fromString("{{hostAddress}}");
}

export function getResolverAddress(): Address {
    return Address.fromString("{{resolverV1Address}}");
}

export function getNativeAssetSuperTokenAddress(): Address {
    return Address.fromString("{{nativeAssetSuperTokenAddress}}");
}
