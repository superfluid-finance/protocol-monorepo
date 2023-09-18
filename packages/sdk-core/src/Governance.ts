import { ethers } from "ethers";

import {
    IGetGovernanceParametersParams,
    IWeb3GovernanceParams,
} from "./interfaces";
import {
    SuperfluidGovernanceII,
    SuperfluidGovernanceII__factory,
} from "./typechain-types";

export default class Governance {
    contract: SuperfluidGovernanceII;
    hostAddress: string;

    constructor(hostAddress: string, governanceAddress: string) {
        this.contract = new ethers.Contract(
            governanceAddress,
            SuperfluidGovernanceII__factory.abi
        ) as SuperfluidGovernanceII;
        this.hostAddress = hostAddress;
    }

    /**
     * Returns the 3Ps config for the specified token (or default) - the liquidation period and patrician period
     * @see https://docs.superfluid.finance/superfluid/sentinels/liquidations-and-toga
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @param token specified governance parameter token
     * @returns {Object} liquidationPeriod and patricianPeriod as strings
     */
    getPPPConfig = async ({
        providerOrSigner,
        token = ethers.constants.AddressZero,
    }: IGetGovernanceParametersParams) => {
        const { liquidationPeriod, patricianPeriod } = await this.contract
            .connect(providerOrSigner)
            .getPPPConfig(this.hostAddress, token);
        return {
            liquidationPeriod: liquidationPeriod.toString(),
            patricianPeriod: patricianPeriod.toString(),
        };
    };

    /**
     * Returns the reward address for the specified token (or default)
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @param token specified governance parameter token
     * @returns {string} the reward address
     */
    getRewardAddress = async ({
        providerOrSigner,
        token = ethers.constants.AddressZero,
    }: IGetGovernanceParametersParams) => {
        return await this.contract
            .connect(providerOrSigner)
            .getRewardAddress(this.hostAddress, token);
    };

    /**
     * Returns the minimum deposit for the specified token (or default)
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @param token specified governance parameter token
     * @returns {string} minimum deposit
     */
    getMinimumDeposit = async ({
        providerOrSigner,
        token = ethers.constants.AddressZero,
    }: IGetGovernanceParametersParams) => {
        const superTokenMinimumDeposit = await this.contract
            .connect(providerOrSigner)
            .getSuperTokenMinimumDeposit(this.hostAddress, token);
        return superTokenMinimumDeposit.toString();
    };

    /**
     * Returns the relevant governance parameters
     * @param providerOrSigner a provider or signer for executing a web3 call
     * @param token specified governance parameter token
     * @returns {Object} liquidationPeriod, patricianPeriod, rewardAddress and minimumDeposit
     */
    getGovernanceParameters = async ({
        providerOrSigner,
        token = ethers.constants.AddressZero,
    }: IGetGovernanceParametersParams): Promise<IWeb3GovernanceParams> => {
        const pppPromise = this.getPPPConfig({ providerOrSigner, token });
        const rewardPromise = this.getRewardAddress({
            providerOrSigner,
            token,
        });
        const minimumDepositPromise = this.getMinimumDeposit({
            providerOrSigner,
            token,
        });
        const data = await Promise.all([
            pppPromise,
            rewardPromise,
            minimumDepositPromise,
        ]);
        return {
            liquidationPeriod: data[0].liquidationPeriod,
            patricianPeriod: data[0].patricianPeriod,
            rewardAddress: data[1],
            minimumDeposit: data[2],
        };
    };
}
