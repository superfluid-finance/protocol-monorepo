import { ethers } from "ethers";
import { chainIdToAddresses, networkNameToChainIdMap } from "./constants";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { abi as SuperTokenABI } from "./abi/SuperToken.json";
import { abi as IConstantFlowAgreementV1 } from "./abi/IConstantFlowAgreementV1.json";
import { getNetworkName } from "./frameworkHelpers";
import { ISuperfluid, ISuperToken } from "./typechain";
import { ChainId, ICreateFlowParams, NetworkName } from "./interfaces";
import Operation from "./Operation";
import { normalizeAddress } from "./utils";

export interface ITokenConstructorOptions {
    readonly address: string;
    readonly chainId?: ChainId;
    readonly networkName?: NetworkName;
}
export interface ITokenOptions {
    readonly address: string;
    readonly chainId: ChainId;
    readonly networkName: NetworkName;
}

const cfaInterface = new ethers.utils.Interface(IConstantFlowAgreementV1);
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
            networkName,
        };
    }

    private superTokenContract = (signer?: ethers.Signer) => {
        return new ethers.Contract(
            this.options.address,
            SuperTokenABI,
            signer
        ) as ISuperToken;
    };

    // SuperToken Contract Read Functions
    balanceOf = async (address: string, provider: ethers.Signer) => {
        return await this.superTokenContract(provider).balanceOf(address);
    };

    realtimeBalanceOf = async (
        address: string,
        timestamp: string,
        provider: ethers.Signer
    ) => {
        return await this.superTokenContract(provider).realtimeBalanceOf(
            address,
            timestamp
        );
    };

    realtimeBalanceOfNow = async (address: string, provider: ethers.Signer) => {
        return await this.superTokenContract(provider).realtimeBalanceOfNow(
            address
        );
    };

    // SuperToken Contract Write Functions
    approve = async (
        recipient: string,
        amount: string,
        signer: ethers.Signer
    ) => {
        try {
            return await this.superTokenContract(signer).approve(
                recipient,
                amount
            );
        } catch (err) {
            throw new Error(JSON.stringify(err));
        }
    };

    downgrade = async (amount: string, signer: ethers.Signer) => {
        return await this.superTokenContract(signer).downgrade(amount);
    };

    transfer = async (
        recipient: string,
        amount: string,
        signer: ethers.Signer
    ) => {
        return await this.superTokenContract(signer).transfer(
            recipient,
            amount
        );
    };

    transferFrom = async (
        sender: string,
        recipient: string,
        amount: string,
        signer: ethers.Signer
    ) => {
        return await this.superTokenContract(signer).transferFrom(
            sender,
            recipient,
            amount
        );
    };

    upgrade = async (amount: string, signer: ethers.Signer) => {
        return await this.superTokenContract(signer).upgrade(amount);
    };

    // TODO: move this out to the sf class
    // All classes should just take a config
    hostContract = (signer?: ethers.Signer) => {
        return new ethers.Contract(
            chainIdToAddresses.get(this.options.chainId)!.host,
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
    }: ICreateFlowParams) : Promise<Operation> => {
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
            chainIdToAddresses.get(this.options.chainId)!.cfaV1,
            callData,
            userData ?? "0x", // TODO(KK): Test
            { from: normalizedSender }
        );
        return new Operation(txn);
    };
}
