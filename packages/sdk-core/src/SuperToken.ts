import { ethers } from "ethers";
import { networkNameToChainIdMap } from "./constants";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { abi as IConstantFlowAgreementV1ABI } from "./abi/IConstantFlowAgreementV1.json";
import { getNetworkName } from "./frameworkHelpers";
import {
    Superfluid as ISuperfluid,
    SuperToken as ISuperToken,
} from "./typechain";
import { ChainId, ICreateFlowParams, IConfig, NetworkName } from "./interfaces";
import Operation from "./Operation";
import { normalizeAddress } from "./utils";

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

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1ABI);
// const idaInterface = new ethers.utils.Interface(IInstantDistributionAgreementV1);

export default class SuperToken {
    readonly options: ITokenOptions;

    constructor(options: ITokenConstructorOptions) {
        if (!options.chainId && !options.networkName) {
            throw new Error("You must input chainId or networkName.");
        }
        const networkName = getNetworkName(options);
        this.options = {
            address: options.address,
            chainId:
                options.chainId || networkNameToChainIdMap.get(networkName)!,
            config: options.config,
            networkName,
        };
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
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
            throw new Error(JSON.stringify(err));
        }
    };

    hostContract = (signer?: ethers.Signer) => {
        return new ethers.Contract(
            this.options.config.hostAddress,
            SuperfluidABI,
            signer
        ) as ISuperfluid;
    };

    // TODO: abstract the CFA data into its own class
    // do the same w/ IDA and then just call the functinos from within this class.
    // all you need to initialize the CFA/IDA class is an address
    // createFlow in the token context would just pass in this.options.address as token
    createFlow = async ({
        sender,
        receiver,
        flowRate,
        userData,
    }: ICreateFlowParams): Promise<Operation> => {
        // TODO: check if address, if not throw error else
        // normalize to lowercase internally
        const normalizedToken = normalizeAddress(this.options.address);
        const normalizedSender = normalizeAddress(sender);
        const normalizedReceiver = normalizeAddress(receiver);

        const callData = cfaInterface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);
        const hostContract = this.hostContract();
        const txn = await hostContract.populateTransaction.callAgreement(
            this.options.config.cfaV1Address,
            callData,
            userData || "0x", // TODO(KK): Test
            { from: normalizedSender }
        );
        return new Operation(txn);
    };
}
