import { ethers } from "ethers";

import SuperfluidGovernanceIIABI from "./abi/SuperfluidGovernanceII.json";
import {
    IGetGovernanceParametersParams,
    IWeb3GovernanceParams,
} from "./interfaces";
import { SuperfluidGovernanceII } from "./typechain/SuperfluidGovernanceII";

export default class Governance {
    contract: SuperfluidGovernanceII;
    hostAddress: string;

    constructor(govAddress: string, hostAddress: string) {
        this.contract = new ethers.Contract(
            govAddress,
            SuperfluidGovernanceIIABI.abi
        ) as SuperfluidGovernanceII;
        this.hostAddress = hostAddress;
    }

    /**
     * Returns the relevant governance parameters
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @param tokenAddress governance parameter for a specific token address
     * @returns
     */
    getGovernanceParameters = async ({
        providerOrSigner,
        tokenAddress = ethers.constants.AddressZero,
    }: IGetGovernanceParametersParams): Promise<IWeb3GovernanceParams> => {
        const { liquidationPeriod, patricianPeriod } = await this.contract
            .connect(providerOrSigner)
            .getPPPConfig(this.hostAddress, tokenAddress);
        const rewardAddress = await this.contract
            .connect(providerOrSigner)
            .getRewardAddress(this.hostAddress, tokenAddress);
        const superTokenMinimumDeposit = await this.contract
            .connect(providerOrSigner)
            .getSuperTokenMinimumDeposit(this.hostAddress, tokenAddress);
        return {
            liquidationPeriod: liquidationPeriod.toString(),
            patricianPeriod: patricianPeriod.toString(),
            rewardAddress,
            superTokenMinimumDeposit: superTokenMinimumDeposit.toString(),
        };
    };
}
