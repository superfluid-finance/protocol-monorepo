import { JsonFragment } from "@ethersproject/abi";
import { ethers } from "ethers";
import { IIndexSubscription } from "./interfaces";
import {
    DAYS_PER_MONTH,
    HOURS_PER_DAY,
    MINUTES_PER_HOUR,
    MONTHS_PER_YEAR,
    SECONDS_PER_MINUTE,
} from "./constants";
import SFError from "./SFError";

const EMPTY = "0x";

/**
 * @dev Checks if address is a valid ethereum address and normalizes so it can be used by both subgraph and web3.
 * @param address
 * @returns The normalized address.
 */
export const normalizeAddress = (address?: string): string => {
    if (!address) return "";
    if (ethers.utils.isAddress(address) === false) {
        throw new SFError({
            type: "INVALID_ADDRESS",
            customMessage:
                "The address you have entered is not a valid ethereum address.",
        });
    }

    return address.toLowerCase();
};

export const isNullOrEmpty = (str: string | null | undefined) => {
    return str == null || str === "";
};

/**
 * @dev Removes the 8-character signature hash from `callData`.
 * @param callData
 * @returns function parameters
 */
export const removeSigHashFromCallData = (callData: string) =>
    EMPTY.concat(callData.slice(10));

/**
 * @dev A wrapper function for getting the ethers TransactionDescription object given fragments (e.g. ABI), callData and the value amount sent.
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

/**
 * @dev Gets the per second flow rate given an `amountPerYear` value.
 * @param amountPerYear the amount you want to stream per year
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByYear = (amountPerYear: string) => {
    return Math.round(
        Number(amountPerYear) *
            MONTHS_PER_YEAR *
            DAYS_PER_MONTH *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    ).toString();
};

/**
 * @dev Gets the per second flow rate given an `amountPerMonth` value.
 * @param amountPerMonth the amount you want to stream per month
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByMonth = (amountPerMonth: string) => {
    return Math.round(
        Number(amountPerMonth) *
            DAYS_PER_MONTH *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    ).toString();
};

/**
 * @dev Gets the per second flow rate given an `amountPerDay` value.
 * @param amountPerDay the amount you want to stream per day
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByDay = (amountPerDay: string) => {
    return Math.round(
        Number(amountPerDay) *
            HOURS_PER_DAY *
            MINUTES_PER_HOUR *
            SECONDS_PER_MINUTE
    ).toString();
};

/**
 * @dev The formula for calculating the flowed amount since updated using Subgraph data.
 * @param netFlowRate the net flow rate of the user
 * @param currentTimestamp the current timestamp
 * @param updatedAtTimestamp the updated at timestamp of the `AccountTokenSnapshot` entity
 * @returns the flowed amount since the updatedAt timestamp
 */
export const flowedAmountSinceUpdatedAt = ({
    netFlowRate,
    currentTimestamp,
    updatedAtTimestamp,
}: {
    netFlowRate: string;
    currentTimestamp: string;
    updatedAtTimestamp: string;
}) => {
    return (
        (Number(currentTimestamp) - Number(updatedAtTimestamp)) *
        Number(netFlowRate)
    );
};

/**
 * @dev The formula for calculating the total amount distributed to the subscriber (pending or received).
 * @param indexSubscriptions the index subscriptions of a single token from an account.
 * @returns the total amount received since updated at (both pending and actually distributed)
 */
export const subscriptionTotalAmountDistributedSinceUpdated = (
    indexSubscriptions: IIndexSubscription[]
) => {
    return indexSubscriptions.reduce(
        (x, y) =>
            x +
            (Number(y.index.indexValue) - Number(y.indexValueUntilUpdatedAt)) *
                Number(y.units),
        0
    );
};

/**
 * @dev The formula for calculating the total amount received (approved subscriptions).
 * @param indexSubscriptions the index subscriptions of a single token from an account.
 * @returns the total amount received since updated at (actually distributed into wallet)
 */
export const subscriptionTotalAmountReceivedSinceUpdated = (
    indexSubscriptions: IIndexSubscription[]
) => {
    return indexSubscriptions
        .filter((x) => x.approved)
        .reduce(
            (x, y) =>
                x +
                (Number(y.index.indexValue) -
                    Number(y.indexValueUntilUpdatedAt)) *
                    Number(y.units),
            0
        );
};

/**
 * @dev The formula for calculating the total amount that is claimable.
 * @param indexSubscriptions the index subscriptions of a single token from an account.
 * @returns the total amount that can be claimed since updated at
 */
export const subscriptionTotalAmountClaimableSinceUpdatedAt = (
    indexSubscriptions: IIndexSubscription[]
) => {
    return (
        subscriptionTotalAmountDistributedSinceUpdated(indexSubscriptions) -
        subscriptionTotalAmountReceivedSinceUpdated(indexSubscriptions)
    );
};

/**
 * @dev The formula for calculating the balance until updated at of a user (claimable + received tokens from index)
 * @param currentBalance the current balance until updated at from the `AccountTokenSnapshot` entity
 * @param netFlowRate the net flow rate of the user
 * @param currentTimestamp the current timestamp
 * @param updatedAtTimestamp the updated at timestamp of the `AccountTokenSnapshot` entity
 * @returns the balance since the updated at timestamp
 */
export const getBalance = ({
    currentBalance,
    netFlowRate,
    currentTimestamp,
    updatedAtTimestamp,
    indexSubscriptions,
}: {
    currentBalance: string;
    netFlowRate: string;
    currentTimestamp: string;
    updatedAtTimestamp: string;
    indexSubscriptions: IIndexSubscription[];
}) => {
    return (
        Number(currentBalance) +
        flowedAmountSinceUpdatedAt({
            netFlowRate,
            currentTimestamp,
            updatedAtTimestamp,
        }) +
        subscriptionTotalAmountReceivedSinceUpdated(indexSubscriptions)
    );
};
