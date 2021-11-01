import { ethers } from "ethers";
import { networkNameToChainIdMap } from "./constants";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { getNetworkName } from "./frameworkHelpers";
import {
    Superfluid as ISuperfluid,
    SuperToken as ISuperToken,
} from "./typechain";
import { ChainId, IConfig, NetworkName } from "./interfaces";
import Operation from "./Operation";
import { handleError } from "./errorHelper";
import ConstantFlowAgreementV1 from "./ConstantFlowAgreementV1";
import { ISuperTokenCreateFlowParams } from ".";

export interface ITokenConstructorOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId?: ChainId;
    readonly networkName?: NetworkName;
}
export interface ITokenOptions {
    readonly address: string;
    readonly config: IConfig;
    readonly chainId: ChainId;
    readonly networkName: NetworkName;
}

// const idaInterface = new ethers.utils.Interface(IInstantDistributionAgreementV1);

export default class SuperToken {
    readonly options: ITokenOptions;
    readonly constantFlowAgreementV1: ConstantFlowAgreementV1;

    constructor(options: ITokenConstructorOptions) {
        if (!options.chainId && !options.networkName) {
            handleError(
                "SUPERTOKEN_INITIALIZATION",
                "You must input chainId or networkName."
            );
        }
        const networkName = getNetworkName(options);
        this.options = {
            address: options.address,
            chainId:
                options.chainId || networkNameToChainIdMap.get(networkName)!,
            config: options.config,
            networkName,
        };
        this.constantFlowAgreementV1 = new ConstantFlowAgreementV1({
            config: this.options.config,
            hostContract: new ethers.Contract(
                this.options.config.hostAddress,
                SuperfluidABI
            ) as ISuperfluid,
        });
    }

    private get superTokenContract() {
        return new ethers.Contract(
            this.options.address,
            SuperTokenABI
        ) as ISuperToken;
    }

    // SuperToken Contract Read Functions
    balanceOf = async (address: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.balanceOf(
                    address
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting balanceOf",
                JSON.stringify(err)
            );
        }
    };

    realtimeBalanceOf = async (address: string, timestamp: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.realtimeBalanceOf(
                    address,
                    timestamp
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOf",
                JSON.stringify(err)
            );
        }
    };

    realtimeBalanceOfNow = async (address: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.realtimeBalanceOfNow(
                    address
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_READ",
                "There was an error getting realtimeBalanceOfNow",
                JSON.stringify(err)
            );
        }
    };

    // SuperToken Contract Write Functions
    approve = async (recipient: string, amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.approve(
                    recipient,
                    amount
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_WRITE",
                "There was an error approving token spend",
                JSON.stringify(err)
            );
        }
    };

    downgrade = async (amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.downgrade(
                    amount
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_WRITE",
                "There was an error downgrading the token",
                JSON.stringify(err)
            );
        }
    };

    transfer = async (recipient: string, amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.transfer(
                    recipient,
                    amount
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_WRITE",
                "There was an error transferring the token",
                JSON.stringify(err)
            );
        }
    };

    transferFrom = async (
        sender: string,
        recipient: string,
        amount: string
    ) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.transferFrom(
                    sender,
                    recipient,
                    amount
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_WRITE",
                "There was an error transferring (transferFrom) the token",
                JSON.stringify(err)
            );
        }
    };

    upgrade = async (amount: string) => {
        try {
            const txn =
                await this.superTokenContract.populateTransaction.upgrade(
                    amount
                );
            return new Operation(txn);
        } catch (err) {
            return handleError(
                "SUPERTOKEN_WRITE",
                "There was an error upgrading the token",
                JSON.stringify(err)
            );
        }
    };

    // CFA Functions
    /**
     * Create a flow of the token of this class.
     * @param {{ sender: string, receiver: string, flowRate: string, userData?: string }}
     * @returns {Promise<Operation>}
     */
    createFlow = async ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ISuperTokenCreateFlowParams): Promise<Operation> => {
        return await this.constantFlowAgreementV1.createFlow({
            flowRate,
            receiver,
            sender,
            token: this.options.address,
            userData,
        });
    };
}
