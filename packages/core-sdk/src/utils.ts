import { ethers } from "ethers";

export const normalizeAddressForContract = (address: string) => {
    return ethers.utils.getAddress(address);
};

export const normalizeAddressForSubgraph = (address: string) => {
    return address.toLowerCase();
};
