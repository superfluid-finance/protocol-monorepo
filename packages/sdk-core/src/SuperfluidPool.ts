import {
    ISuperfluidPool,
    ISuperfluidPool__factory,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ContractTransaction, ethers } from "ethers";

import {
    ERC20AllowanceParams,
    ERC20ApproveParams,
    ERC20DecreaseAllowanceParams,
    ERC20IncreaseAllowanceParams,
    ERC20TransferFromParams,
    ERC20TransferParams,
    GetClaimableParams,
    UpdateMemberParams,
} from "./interfaces";
import { normalizeAddress } from "./utils";

/**
 * Superfluid Pool Helper Class
 * @description A helper class to interact with the SuperfluidPool contract.
 */
export default class SuperfluidPool {
    readonly contract: ISuperfluidPool;

    constructor(poolAddress: string) {
        this.contract = new ethers.Contract(
            poolAddress,
            ISuperfluidPool__factory.abi
        ) as ISuperfluidPool;
    }

    /**
     * Retrieves the pool admin.
     * @returns The pool admin.
     */
    getPoolAdmin = async (): Promise<string> => {
        return await this.contract.admin();
    };

    /**
     * Retrieves the SuperToken.
     * @returns The SuperToken for this pool.
     */
    getSuperToken = async (): Promise<string> => {
        return await this.contract.superToken();
    };

    /**
     * Retrieves the total units.
     * Returns the same value as totalSupply.
     * @returns The total units.
     */
    getTotalUnits = async (): Promise<string> => {
        return (await this.contract.getTotalUnits()).toString();
    };

    /**
     * Retrieves the total connected units.
     * @returns The total connected units.
     */
    getTotalConnectedUnits = async (): Promise<string> => {
        return (await this.contract.getTotalConnectedUnits()).toString();
    };

    /**
     * Retrieves the units for a specific member.
     * @param member The member's address.
     * @returns The units for the specified member.
     */
    getUnits = async (member: string): Promise<string> => {
        return (await this.contract.getUnits(member)).toString();
    };

    /**
     * Retrieves the total connected flow rate.
     * @returns The total connected flow rate.
     */
    getTotalConnectedFlowRate = async (): Promise<string> => {
        return (await this.contract.getTotalConnectedFlowRate()).toString();
    };

    /**
     * Retrieves the total disconnected flow rate.
     * @returns The total disconnected flow rate.
     */
    getTotalDisconnectedFlowRate = async (): Promise<string> => {
        return (await this.contract.getTotalDisconnectedFlowRate()).toString();
    };

    /**
     * Retrieves the disconnected balance for a specific time.
     * @param time The time of the disconnected balance.
     * @returns The disconnected balance.
     */
    getDisconnectedBalance = async (time: string): Promise<string> => {
        return (await this.contract.getDisconnectedBalance(time)).toString();
    };

    /**
     * Retrieves the flow rate for a specific member.
     * @param member The member's address.
     * @returns The flow rate for the specified member.
     */
    getMemberFlowRate = async (member: string): Promise<string> => {
        return (await this.contract.getMemberFlowRate(member)).toString();
    };

    /**
     * Retrieves the claimable amount for a specific member and time.
     * @param member The member's address.
     * @param time The amount claimable at time.
     * @returns The claimable amount.
     */
    getClaimable = async (params: GetClaimableParams): Promise<string> => {
        const normalizedMember = normalizeAddress(params.member);
        return (
            await this.contract.getClaimable(normalizedMember, params.time)
        ).toString();
    };

    /**
     * Retrieves the claimable amount for a specific member at the current time.
     * @param member The member's address.
     * @returns The claimable amount.
     */
    getClaimableNow = async (member: string): Promise<string> => {
        return (await this.contract.getClaimableNow(member)).toString();
    };

    /**
     * Updates the units for a specific member.
     * @param member The member's address.
     * @param newUnits The new units value.
     * @returns A promise that resolves when the update is complete.
     */
    updateMember = async (
        params: UpdateMemberParams
    ): Promise<ContractTransaction> => {
        const normalizedMember = normalizeAddress(params.member);
        return await this.contract.updateMember(
            normalizedMember,
            params.newUnits
        );
    };

    /**
     * Claims all available funds for a specific member.
     * @param member The member's address.
     * @returns A promise that resolves when the claim is complete.
     */
    claimAllForMember = async (
        member: string
    ): Promise<ContractTransaction> => {
        return await this.contract["claimAll(address)"](member);
    };

    /**
     * Claims all available funds.
     * @returns A promise that resolves when the claim is complete.
     */
    claimAll = async (): Promise<ContractTransaction> => {
        return await this.contract["claimAll()"]();
    };

    /**
     * Increases the allowance for a specific spender.
     * @param spender The spender's address.
     * @param amount The amount to increase the allowance by.
     * @returns A promise that resolves when the allowance increase is complete.
     */
    increaseAllowance = async (
        params: ERC20IncreaseAllowanceParams
    ): Promise<ContractTransaction> => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.increaseAllowance(
            normalizedSpender,
            params.amount,
            params.overrides
        );
    };

    /**
     * Decreases the allowance for a specific spender.
     * @param spender The spender's address.
     * @param amount The amount to decrease the allowance by.
     * @returns A promise that resolves when the allowance decrease is complete.
     */
    decreaseAllowance = async (
        params: ERC20DecreaseAllowanceParams
    ): Promise<ContractTransaction> => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.decreaseAllowance(
            normalizedSpender,
            params.amount,
            params.overrides
        );
    };

    /**
     * Retrieves the total supply.
     * Returns the same value as getTotalUnits.
     * @returns The total supply.
     */
    totalSupply = async (): Promise<string> => {
        return (await this.contract.totalSupply()).toString();
    };

    /**
     * Retrieves the balance of an account.
     * @param account The account's address.
     * @returns The account's balance.
     */
    balanceOf = async (account: string): Promise<string> => {
        const normalizedAccount = normalizeAddress(account);
        return (await this.contract.balanceOf(normalizedAccount)).toString();
    };

    /**
     * Retrieves the allowance for a specific owner and spender.
     * @param owner The owner's address.
     * @param spender The spender's address.
     * @returns The allowance.
     */
    allowance = async (params: ERC20AllowanceParams): Promise<string> => {
        const normalizedOwner = normalizeAddress(params.owner);
        const normalizedSpender = normalizeAddress(params.spender);
        return (
            await this.contract.allowance(normalizedOwner, normalizedSpender)
        ).toString();
    };

    /**
     * Approves an amount to be spent by a specific spender.
     * @param spender The spender's address.
     * @returns A promise that resolves when the approval is complete.
     */
    approve = async (
        params: ERC20ApproveParams
    ): Promise<ContractTransaction> => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.approve(normalizedSpender, params.amount);
    };

    /**
     * Transfers an amount to a specific recipient.
     * @param to The recipient's address.
     * @param amount The amount to transfer.
     * @returns A promise that resolves when the transfer is complete.
     */
    transfer = async (
        params: ERC20TransferParams
    ): Promise<ContractTransaction> => {
        const normalizedTo = normalizeAddress(params.to);
        return await this.contract.transfer(normalizedTo, params.amount);
    };

    /**
     * Transfers an amount from a specific sender to a recipient.
     * @param from The sender's address.
     * @param to The recipient's address.
     * @param amount The amount to transfer.
     * @returns A promise that resolves when the transfer is complete.
     */
    transferFrom = async (
        params: ERC20TransferFromParams
    ): Promise<ContractTransaction> => {
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedTo = normalizeAddress(params.to);
        return await this.contract.transferFrom(
            normalizedFrom,
            normalizedTo,
            params.amount
        );
    };
}
