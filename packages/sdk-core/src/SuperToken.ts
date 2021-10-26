import { ethers } from "ethers";
import { ChainId, NetworkName } from ".";
import constantFlowAgreementV1ABI from "./abi/IConstantFlowAgreementV1.json";
import { chainIdToAddresses, networkNameToChainIdMap } from "./constants";
import { getNetworkName } from "./frameworkHelpers";
import { normalizeAddressForContract } from "./utils";
import { ISuperfluid } from "./typechain";

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

    hostContract(signer: ethers.Signer | ethers.providers.Provider) {
        return new ethers.Contract(
            chainIdToAddresses.get(this.options.chainId)!.host,
            constantFlowAgreementV1ABI,
            signer
        ) as ISuperfluid;
    }

    createFlow = async ({
        sender,
        receiver,
        flowRate,
        userData,
        signer,
    }: {
        sender: string;
        receiver: string;
        flowRate: string;
        userData: string;
        signer: ethers.Signer | ethers.providers.Provider;
    }) => {
        const normalizedToken = normalizeAddressForContract(
            this.options.address
        );
        const normalizedSender = normalizeAddressForContract(sender);
        const normalizedReceiver = normalizeAddressForContract(receiver);

        const ABI = [
            "function createFlow(address token,address receiver,int96 flowRate,bytes ctx)",
        ];
        const iface = new ethers.utils.Interface(ABI);
        const callData = iface.encodeFunctionData("createFlow", [
            normalizedToken,
            normalizedReceiver,
            flowRate,
            "0x",
        ]);
        const hostContract = this.hostContract(signer);
        return await hostContract.callAgreement(
            chainIdToAddresses.get(this.options.chainId)!.cfaV1,
            callData,
            userData,
            { from: normalizedSender }
        );
    };
}
