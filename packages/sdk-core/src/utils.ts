import { ethers } from "ethers";
import { IPaginateResponse, IPaginateRequest } from "./interfaces";

export const normalizeAddressForContract = (address?: string) => {
    if (!address) return "";

    return ethers.utils.getAddress(address);
};

export const normalizeAddressForSubgraph = (address?: string) => {
    if (!address) return "";

    return address.toLowerCase();
};

export const buildWhereForSubgraphQuery = <T>(data: T) => {
    return Object.entries(data)
        .filter((x) => x[1] != null && x[1] !== "")
        .map((x) => `${[x[0]]}: "${normalizeAddressForSubgraph(x[1])}"`)
        .join(",");
};

export const defaultPaginateOptions = (
    options: IPaginateRequest
): IPaginateResponse => {
    return {
        first: options.first == null ? 100 : options.first,
        skip: options.skip == null ? 0 : options.skip,
    };
};
