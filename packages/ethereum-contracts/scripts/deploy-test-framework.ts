import {Contract, ethers} from "ethers";
import {ethers as hardhatEthers} from "hardhat";
import {Artifact, FactoryOptions} from "hardhat/types";

import SlotsBitmapLibraryArtifact from "../artifacts/contracts/libs/SlotsBitmapLibrary.sol/SlotsBitmapLibrary.json";
import SuperfluidFrameworkDeployerArtifact from "../artifacts/contracts/utils/SuperfluidFrameworkDeployer.sol/SuperfluidFrameworkDeployer.json";
import {
    SlotsBitmapLibrary,
    SuperfluidFrameworkDeployer,
} from "../typechain-types";
import ERC1820Registry from "./artifacts/ERC1820Registry.json";

const ERC1820_ADDRESS = "0x1820a4b7618bde71dce8cdc73aab6c95905fad24";
const ERC1820_DEPLOYER = "0xa990077c3205cbDf861e17Fa532eeB069cE9fF96";
const ERC1820_BIN = ERC1820Registry.bin;
const ERC1820_PAYLOAD =
    "0xf90a388085174876e800830c35008080b909e5" +
    ERC1820_BIN +
    "1ba01820182018201820182018201820182018201820182018201820182018201820a01820182018201820182018201820182018201820182018201820182018201820";

export async function deployERC1820(
    provider: ethers.providers.JsonRpcProvider
): Promise<void> {
    console.log("Deploying ERC1820...");
    const code = await provider.send("eth_getCode", [
        ERC1820_ADDRESS,
        "latest",
    ]);
    if (code === "0x") {
        const [from] = await provider.send("eth_accounts", []);

        const tx = await provider.send("eth_sendTransaction", [
            {
                from,
                to: ERC1820_DEPLOYER,
                value: "0x11c37937e080000",
            },
        ]);

        await provider.send("eth_sendRawTransaction", [ERC1820_PAYLOAD]);

        console.log("ERC1820 registry successfully deployed");
    }
}

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
