import {
    ISuperfluidPool,
    ISuperfluidPool__factory,
} from "@superfluid-finance/ethereum-contracts/build/typechain";
import { ethers } from "ethers";

import {
    ERC20AllowanceParams,
    ERC20ApproveParams,
    ERC20DecreaseAllowanceParams,
    ERC20IncreaseAllowanceParams,
    ERC20TransferFromParams,
    ERC20TransferParams,
    GetClaimableParams,
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

    getPoolAdmin = async () => {
        return await this.contract.admin();
    };

    getSuperToken = async () => {
        return await this.contract.superToken();
    };

    getTotalUnits = async () => {
        return (await this.contract.getTotalUnits()).toString();
    };

    getTotalConnectedUnits = async () => {
        return (await this.contract.getTotalConnectedUnits()).toString();
    };

    getUnits = async (member: string) => {
        return (await this.contract.getUnits(member)).toString();
    };

    getTotalConnectedFlowRate = async () => {
        return (await this.contract.getTotalConnectedFlowRate()).toString();
    };

    getTotalDisconnectedFlowRate = async () => {
        return (await this.contract.getTotalDisconnectedFlowRate()).toString();
    };

    getDisconnectedBalance = async (time: string) => {
        return (await this.contract.getDisconnectedBalance(time)).toString();
    };

    getMemberFlowRate = async (member: string) => {
        return (await this.contract.getMemberFlowRate(member)).toString();
    };

    getClaimable = async (params: GetClaimableParams) => {
        const normalizedMember = normalizeAddress(params.member);

        return (
            await this.contract.getClaimable(normalizedMember, params.time)
        ).toString();
    };

    getClaimableNow = async (member: string) => {
        return (await this.contract.getClaimableNow(member)).toString();
    };

    updateMember = async (member: string, newUnits: string) => {
        return await this.contract.updateMember(member, newUnits);
    };

    claimAllForMember = async (member: string) => {
        return await this.contract["claimAll(address)"](member);
    };

    claimAll = async () => {
        return await this.contract["claimAll()"]();
    };

    // ERC20 Functions

    increaseAllowance = async (params: ERC20IncreaseAllowanceParams) => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.increaseAllowance(
            normalizedSpender,
            params.amount,
            params.overrides
        );
    };

    decreaseAllowance = async (params: ERC20DecreaseAllowanceParams) => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.decreaseAllowance(
            normalizedSpender,
            params.amount,
            params.overrides
        );
    };

    totalSupply = async () => {
        return (await this.contract.totalSupply()).toString();
    };

    balanceOf = async (account: string) => {
        const normalizedAccount = normalizeAddress(account);
        return (await this.contract.balanceOf(normalizedAccount)).toString();
    };

    allowance = async (params: ERC20AllowanceParams) => {
        const normalizedOwner = normalizeAddress(params.owner);
        const normalizedSpender = normalizeAddress(params.spender);
        return (
            await this.contract.allowance(normalizedOwner, normalizedSpender)
        ).toString();
    };

    approve = async (params: ERC20ApproveParams) => {
        const normalizedSpender = normalizeAddress(params.spender);
        return await this.contract.approve(normalizedSpender, params.amount);
    };

    transfer = async (params: ERC20TransferParams) => {
        const normalizedTo = normalizeAddress(params.to);
        return await this.contract.transfer(normalizedTo, params.amount);
    };

    transferFrom = async (params: ERC20TransferFromParams) => {
        const normalizedFrom = normalizeAddress(params.from);
        const normalizedTo = normalizeAddress(params.to);
        return await this.contract.transferFrom(
            normalizedFrom,
            normalizedTo,
            params.amount
        );
    };
}
