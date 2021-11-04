import { JsonFragment } from "@ethersproject/abi";
import { ethers } from "ethers";
import {
    DAYS_PER_MONTH,
    HOURS_PER_DAY,
    MINUTES_PER_HOUR,
    MONTHS_PER_YEAR,
    SECONDS_PER_MINUTE,
} from "./constants";
import { handleError } from "./errorHelper";

const EMPTY = "0x";

/**
 * @dev Normalizes ethereum addresses for use in sdk-core.
 * Checks if address is a valid ethereum address,
 * throws an error if it is not, else, normalizes
 * addresses by converting to lower case so it can be used
 * by both the subgraph and web 3 calls.
 * @param address
 * @returns {string} The normalized address.
 */
export const normalizeAddress = (address?: string): string => {
    if (!address) return "";
    if (ethers.utils.isAddress(address) === false) {
        handleError(
            "INVALID_ADDRESS",
            "The address you have entered is not a valid ethereum address."
        );
    }

    return address.toLowerCase();
};

export const isNullOrEmpty = (str: string | null | undefined) => {
    return str == null || str === "";
};

/**
 * Removes the 8-character signature hash from `callData`.
 * @param callData
 * @returns function parameters
 */
export const removeSigHashFromCallData = (callData: string) =>
    EMPTY.concat(callData.slice(10));

/**
 * A wrapper function for getting the ethers TransactionDescription
 * object given fragments (e.g. ABI), callData and the value amount sent.
 * @param fragments ABI
 * @param data callData of a function
 * @param value amount of ether sent
 * @returns ethers.TransactionDescription object
 */
export const getTransactionDescription = (
    fragments:
        | string
        | readonly (string | ethers.utils.Fragment | JsonFragment)[],
    data: string,
    value?: string
) => {
    const iface = new ethers.utils.Interface(fragments);
    const txnDescription = iface.parseTransaction({ data, value });
    return txnDescription;
};

export const getPerSecondFlowRateByYear = (amountPerYear: string) => {
    return Math.round(
        Number(amountPerYear) *
            MONTHS_PER_YEAR *
            DAYS_PER_MONTH *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    );
};

export const getPerSecondFlowRateByMonth = (amountPerMonth: string) => {
    return Math.round(
        Number(amountPerMonth) *
            DAYS_PER_MONTH *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    );
};

export const getPerSecondFlowRateByDay = (amountPerDay: string) => {
    return Math.round(
        Number(amountPerDay) *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    );
};
