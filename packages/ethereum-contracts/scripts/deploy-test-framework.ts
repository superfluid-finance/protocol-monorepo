import {Contract, ethers} from "ethers";
import {ethers as hardhatEthers} from "hardhat";
import {Artifact, FactoryOptions} from "hardhat/types";

import SlotsBitmapLibraryArtifact from "../artifacts/contracts/libs/SlotsBitmapLibrary.sol/SlotsBitmapLibrary.json";
import SuperfluidFrameworkDeployerArtifact from "../artifacts/contracts/utils/SuperfluidFrameworkDeployer.sol/SuperfluidFrameworkDeployer.json";
import {
    SlotsBitmapLibrary,
    SuperfluidFrameworkDeployer,
} from "../typechain-types";
import {deployERC1820} from "./deploy-erc1820";

const _getFactoryAndReturnDeployedContract = async <T extends Contract>(
    contractName: string,
    artifact: Artifact,
    signerOrOptions?: ethers.Signer | FactoryOptions | undefined,
    ...args: any[]
) => {
    console.log(`Deploying ${contractName}...`);
    const ContractFactory = await hardhatEthers.getContractFactoryFromArtifact(
        artifact,
        signerOrOptions
    );
    const contract = (await ContractFactory.deploy(...args)) as T;
    await contract.deployed();
    console.log(`${contractName} Deployed At:`, contract.address);
    return contract;
};

export const deployTestFramework = async () => {
    const signer = (await hardhatEthers.getSigners())[0];
    await deployERC1820(hardhatEthers.provider);
    const SlotsBitmapLibrary =
        await _getFactoryAndReturnDeployedContract<SlotsBitmapLibrary>(
            "SlotsBitmapLibrary",
            SlotsBitmapLibraryArtifact,
            signer
        );
    const frameworkDeployer =
        await _getFactoryAndReturnDeployedContract<SuperfluidFrameworkDeployer>(
            "SuperfluidFrameworkDeployer",
            SuperfluidFrameworkDeployerArtifact,
            {
                signer,
                libraries: {
                    SlotsBitmapLibrary: SlotsBitmapLibrary.address,
                },
            }
        );
    return frameworkDeployer;
};
