import { ethers } from "ethers";
import { chainIdToAddresses, networkNameToChainIdMap } from "./constants";
import { abi as SuperfluidABI } from "./abi/Superfluid.json";
import { abi as IConstantFlowAgreementV1 } from "./abi/IConstantFlowAgreementV1.json";
import { getNetworkName } from "./frameworkHelpers";
import { normalizeAddressForContract } from "./utils";
import { ISuperfluid } from "./typechain";
import { ChainId, NetworkName } from "./interfaces";

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

    hostContract(signer: ethers.Signer | ethers.providers.Provider) {
        return new ethers.Contract(
            chainIdToAddresses.get(this.options.chainId)!.host,
            SuperfluidABI,
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

        const callData = cfaInterface.encodeFunctionData("createFlow", [
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
