import { JsonFragment } from "@ethersproject/abi";
import { HardhatEthersHelpers } from "@nomiclabs/hardhat-ethers/types";
import { BigNumber, ethers } from "ethers";
import Web3 from "web3";

import { SFError } from "./SFError";
import {
    AUTHORIZE_FLOW_OPERATOR_CREATE,
    AUTHORIZE_FLOW_OPERATOR_DELETE,
    AUTHORIZE_FLOW_OPERATOR_UPDATE,
    BASE_18,
    DAY_IN_SECONDS,
    MONTH_IN_SECONDS,
    WEEK_IN_SECONDS,
    YEAR_IN_SECONDS,
} from "./constants";
import { IIndexSubscription } from "./interfaces";

const EMPTY = "0x";

/**
 * Checks if address is a valid ethereum address and normalizes so it can be used by both subgraph and web3.
 * @param address
 * @returns The normalized address.
 */
export const normalizeAddress = (address?: string): string => {
    if (!address) return "";
    if (ethers.utils.isAddress(address) === false) {
        throw new SFError({
            type: "INVALID_ADDRESS",
            message:
                "The address you have entered is not a valid ethereum address",
        });
    }

    return address.toLowerCase();
};

export const isPermissionsClean = (permissions: number): boolean => {
    return (
        (permissions &
            ~(
                AUTHORIZE_FLOW_OPERATOR_CREATE |
                AUTHORIZE_FLOW_OPERATOR_UPDATE |
                AUTHORIZE_FLOW_OPERATOR_DELETE
            )) ===
        0
    );
};

export const _addPermissions = (
    oldPermissions: number,
    permissionsDelta: number
): number => {
    return oldPermissions | permissionsDelta;
};

export const _removePermissions = (
    oldPermissions: number,
    permissionsDelta: number
): number => {
    return oldPermissions & ~permissionsDelta;
};

export const isNullOrEmpty = (str: string | null | undefined) => {
    return str == null || str === "";
};

export function toBN(num: any) {
    return ethers.BigNumber.from(num);
}

/**
 * Removes the 8-character (4 byte) signature hash from `callData`.
 * @param callData
 * @returns function parameters
 */
export const removeSigHashFromCallData = (callData: string) =>
    EMPTY.concat(callData.slice(10));

/**
 * A wrapper function for getting the ethers TransactionDescription object given fragments (e.g. ABI), callData and the value amount sent.
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
 * Gets the per second flow rate given an `amountPerYear` value.
 * @param amountPerYear the amount you want to stream per year
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByYear = (amountPerYear: string) => {
    return Math.round(
        (Number(amountPerYear) / YEAR_IN_SECONDS) * BASE_18
    ).toString();
};

/**
 * Gets the per second flow rate given an `amountPerMonth` value.
 * @param amountPerMonth the amount you want to stream per month
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByMonth = (amountPerMonth: string) => {
    return Math.round(
        (Number(amountPerMonth) / MONTH_IN_SECONDS) * BASE_18
    ).toString();
};

/**
 * Gets the per second flow rate given an `amountPerWeek` value.
 * @param amountPerWeek the amount you want to stream per Week
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByWeek = (amountPerWeek: string) => {
    return Math.round(
        (Number(amountPerWeek) / WEEK_IN_SECONDS) * BASE_18
    ).toString();
};

/**
 * Gets the per second flow rate given an `amountPerDay` value.
 * @param amountPerDay the amount you want to stream per day
 * @returns flow rate per second
 */
export const getPerSecondFlowRateByDay = (amountPerDay: string) => {
    return Math.round(
        (Number(amountPerDay) / DAY_IN_SECONDS) * BASE_18
    ).toString();
};

/**
 * Gets daily, weekly, monthly and yearly flowed amounts given a per second flow rate.
 * @param perSecondFlowRate
 * @returns
 */
export const getFlowAmountByPerSecondFlowRate = (perSecondFlowRate: string) => {
    const decimalFlowRate = Number(perSecondFlowRate) / BASE_18;
    return {
        daily: Math.round(decimalFlowRate * DAY_IN_SECONDS).toString(),
        weekly: Math.round(decimalFlowRate * WEEK_IN_SECONDS).toString(),
        monthly: Math.round(decimalFlowRate * MONTH_IN_SECONDS).toString(),
        yearly: Math.round(decimalFlowRate * YEAR_IN_SECONDS).toString(),
    };
};

/**
 * The formula for calculating the flowed amount since updated using Subgraph data.
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
    return toBN(currentTimestamp)
        .sub(toBN(updatedAtTimestamp))
        .mul(toBN(netFlowRate));
};

/**
 * The formula for calculating the total amount distributed to the subscriber (pending or received).
 * @param indexSubscriptions the index subscriptions of a single token from an account.
 * @returns the total amount received since updated at (both pending and actually distributed)
 */
export const subscriptionTotalAmountDistributedSinceUpdated = (
    indexSubscriptions: IIndexSubscription[]
) => {
    return indexSubscriptions.reduce(
        (x, y) =>
            toBN(x)
                .add(
                    toBN(y.index.indexValue).sub(
                        toBN(y.indexValueUntilUpdatedAt)
                    )
                )
                .mul(toBN(y.units)),
        toBN(0)
    );
};

/**
 * The formula for calculating the total amount received (approved subscriptions).
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
                toBN(x)
                    .add(
                        toBN(y.index.indexValue).sub(
                            toBN(y.indexValueUntilUpdatedAt)
                        )
                    )
                    .mul(toBN(y.units)),
            toBN(0)
        );
};

/**
 * The formula for calculating the total amount that is claimable.
 * @param indexSubscriptions the index subscriptions of a single token from an account.
 * @returns the total amount that can be claimed since updated at
 */
export const subscriptionTotalAmountClaimableSinceUpdatedAt = (
    indexSubscriptions: IIndexSubscription[]
) => {
    return subscriptionTotalAmountDistributedSinceUpdated(
        indexSubscriptions
    ).sub(subscriptionTotalAmountReceivedSinceUpdated(indexSubscriptions));
};

export const getStringCurrentTimeInSeconds = () =>
    Math.floor(new Date().getTime() / 1000);

export const getSanitizedTimestamp = (timestamp: ethers.BigNumberish) =>
    new Date(Number(timestamp.toString()) * 1000);

/**
 * The formula for calculating the balance until updated at of a user (claimable + received tokens from index)
 * @param currentBalance the current balance until updated at from the `AccountTokenSnapshot` entity
 * @param netFlowRate the net flow rate of the user
 * @param currentTimestamp the current timestamp
 * @param updatedAtTimestamp the updated at timestamp of the `AccountTokenSnapshot` entity
 * @returns the balance since the updated at timestamp
 */
export const calculateAvailableBalance = ({
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
    return toBN(currentBalance)
        .add(
            flowedAmountSinceUpdatedAt({
                netFlowRate,
                currentTimestamp,
                updatedAtTimestamp,
            })
        )
        .add(subscriptionTotalAmountReceivedSinceUpdated(indexSubscriptions));
};

// NOTE: This is the only places we are allowed to use explicit any in the
// sdk-core otherwise this doesn't work properly.
export const isEthersProvider = (
    provider: any
): provider is ethers.providers.Provider => !!provider.getNetwork;

export const isInjectedWeb3 = (provider: any): provider is Web3 =>
    !!provider.currentProvider;

export const isInjectedEthers = (
    provider: any
): provider is typeof ethers & HardhatEthersHelpers => !!provider.provider;

/**
 * Why? Because `return obj as T` and `return <T>obj` are not safe type casts.
 */
export const typeGuard = <T>(obj: T) => obj;

export const getFlowOperatorId = (sender: string, flowOperator: string) => {
    const encoder = ethers.utils.defaultAbiCoder;
    const encodedData = encoder.encode(
        ["string", "address", "address"],
        ["flowOperator", sender, flowOperator]
    );
    return ethers.utils.keccak256(encodedData);
};

/**
 * Applies clipping to deposit (based on contracts clipping logic)
 * @param deposit the deposit to clip
 * @param roundingDown whether to round up or down
 * @returns clipped deposit
 */
export const clipDepositNumber = (deposit: BigNumber, roundingDown = false) => {
    // last 32 bits of the deposit (96 bits) is clipped off
    const rounding = roundingDown
        ? 0
        : deposit.and(toBN(0xffffffff)).isZero()
        ? 0
        : 1;
    return deposit.shr(32).add(toBN(rounding)).shl(32);
};

export async function tryGet<T>(
    somePromise: Promise<T>,
    defaultReturnValue: T
): Promise<T> {
    try {
        return await somePromise;
    } catch {
        return defaultReturnValue;
    }
}
