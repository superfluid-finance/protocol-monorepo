import { ethers } from "ethers";
import {
    DAYS_PER_MONTH,
    HOURS_PER_DAY,
    MINUTES_PER_HOUR,
    MONTHS_PER_YEAR,
    SECONDS_PER_MINUTE,
} from "./constants";
import { handleError } from "./errorHelper";
import { IPaginateResponse, IPaginateRequest } from "./interfaces";

/**
 * Checks if address is a valid ethereum address and if it is,
 * normalizes addresses for the library by setting them to
 * lower case so it can be used seamlessly by both the
 * subgraph and web 3 calls.
 * @param address
 * @returns
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

export const buildWhereForSubgraphQuery = <T>(data: T) => {
    return Object.entries(data)
        .filter((x) => x[1] != null && x[1] !== "")
        .map((x) => `${[x[0]]}: "${normalizeAddress(x[1])}"`)
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
