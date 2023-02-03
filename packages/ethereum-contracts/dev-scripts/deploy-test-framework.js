const {ethers} = require("hardhat");

const SuperfluidDevNFTDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidDevNFTDeployerLibrary.sol/SuperfluidDevNFTDeployerLibrary.json");
const SuperfluidNFTDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/libs/SuperfluidNFTDeployerLibrary.sol/SuperfluidNFTDeployerLibrary.json");
const SuperfluidGovDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidGovDeployerLibrary.sol/SuperfluidGovDeployerLibrary.json");
const SuperfluidHostDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidHostDeployerLibrary.sol/SuperfluidHostDeployerLibrary.json");
const SuperfluidCFAv1DeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidCFAv1DeployerLibrary.sol/SuperfluidCFAv1DeployerLibrary.json");
const SuperfluidIDAv1DeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidIDAv1DeployerLibrary.sol/SuperfluidIDAv1DeployerLibrary.json");
const SuperfluidPeripheryDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidPeripheryDeployerLibrary.sol/SuperfluidPeripheryDeployerLibrary.json");
const SuperfluidSuperTokenFactoryHelperDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/deployers/SuperfluidSuperTokenFactoryHelperDeployerLibrary.sol/SuperfluidSuperTokenFactoryHelperDeployerLibrary.json");
const SuperfluidFrameworkDeployerArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/SuperfluidFrameworkDeployer.sol/SuperfluidFrameworkDeployer.json");
const SlotsBitmapLibraryArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/libs/SlotsBitmapLibrary.sol/SlotsBitmapLibrary.json");
const SuperTokenDeployerArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/SuperTokenDeployer.sol/SuperTokenDeployer.json");
const TestResolver = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/TestResolver.sol/TestResolver.json");

const ERC1820Registry = require("../ops-scripts/artifacts/ERC1820Registry.json");

const ERC1820_ADDRESS = "0x1820a4b7618bde71dce8cdc73aab6c95905fad24";
const ERC1820_BIN = ERC1820Registry.bin;
const ERC1820_DEPLOYER = "0xa990077c3205cbDf861e17Fa532eeB069cE9fF96";
const ERC1820_PAYLOAD =
    "0xf90a388085174876e800830c35008080b909e5" +
    ERC1820_BIN +
    "1ba01820182018201820182018201820182018201820182018201820182018201820a01820182018201820182018201820182018201820182018201820182018201820";

async function deployERC1820(provider) {
    console.log("Deploying ERC1820...");
    const code = await provider.send("eth_getCode", [
        ERC1820_ADDRESS,
        "latest",
    ]);
    if (code === "0x") {
        const [from] = await provider.send("eth_accounts", []);

        await provider.send("eth_sendTransaction", [
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

const _getFactoryAndReturnDeployedContract = async (
    contractName,
    artifact,
    signerOrOptions,
    ...args
) => {
    console.log(`Deploying ${contractName}...`);
    const ContractFactory = await ethers.getContractFactoryFromArtifact(
        artifact,
        signerOrOptions
    );
    const contract = await ContractFactory.deploy(...args);
    await contract.deployed();
    console.log(`${contractName} Deployed At:`, contract.address);
    return contract;
};

/**
 * Deploys Superfluid Framework in local testing environments.
 * NOTE: This only works with Hardhat.
 * @returns
 */
const deployTestFramework = async () => {
    const signer = (await ethers.getSigners())[0];
    await deployERC1820(ethers.provider);
    const SlotsBitmapLibrary = await _getFactoryAndReturnDeployedContract(
        "SlotsBitmapLibrary",
        SlotsBitmapLibraryArtifact,
        signer
    );
    const SuperfluidDevNFTDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidDevNFTDeployerLibrary",
            SuperfluidDevNFTDeployerLibraryArtifact,
            signer
        );
    const SuperfluidGovDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidGovDeployerLibrary",
            SuperfluidGovDeployerLibraryArtifact,
            signer
        );
    const SuperfluidHostDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidHostDeployerLibrary",
            SuperfluidHostDeployerLibraryArtifact,
            signer
        );
    const SuperfluidCFAv1DeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidCFAv1DeployerLibrary",
            SuperfluidCFAv1DeployerLibraryArtifact,
            signer
        );
    const SuperfluidIDAv1DeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidIDAv1DeployerLibrary",
            SuperfluidIDAv1DeployerLibraryArtifact,
            {
                signer,
                libraries: {
                    SlotsBitmapLibrary: SlotsBitmapLibrary.address,
                },
            }
        );
    const SuperfluidNFTDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidNFTDeployerLibrary",
            SuperfluidNFTDeployerLibraryArtifact,
            signer
        );
    const SuperfluidPeripheryDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidPeripheryDeployerLibrary",
            SuperfluidPeripheryDeployerLibraryArtifact,
            {
                signer,
                libraries: {
                    SuperfluidNFTDeployerLibrary:
                        SuperfluidNFTDeployerLibrary.address,
                },
            }
        );
    const SuperfluidSuperTokenFactoryHelperDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidSuperTokenFactoryHelperDeployerLibrary",
            SuperfluidSuperTokenFactoryHelperDeployerLibraryArtifact
        );
    const frameworkDeployer = await _getFactoryAndReturnDeployedContract(
        "SuperfluidFrameworkDeployer",
        SuperfluidFrameworkDeployerArtifact,
        {
            signer,
            libraries: {
                SuperfluidGovDeployerLibrary:
                    SuperfluidGovDeployerLibrary.address,
                SuperfluidHostDeployerLibrary:
                    SuperfluidHostDeployerLibrary.address,
                SuperfluidCFAv1DeployerLibrary:
                    SuperfluidCFAv1DeployerLibrary.address,
                SuperfluidIDAv1DeployerLibrary:
                    SuperfluidIDAv1DeployerLibrary.address,
                SuperfluidPeripheryDeployerLibrary:
                    SuperfluidPeripheryDeployerLibrary.address,
                SuperfluidSuperTokenFactoryHelperDeployerLibrary:
                    SuperfluidSuperTokenFactoryHelperDeployerLibrary.address,
            },
        }
    );
    const sf = await frameworkDeployer.getFramework();
    const superTokenDeployer = await _getFactoryAndReturnDeployedContract(
        "SuperTokenDeployer",
        SuperTokenDeployerArtifact,
        {
            signer,
            libraries: {
                SuperfluidDevNFTDeployerLibrary:
                    SuperfluidDevNFTDeployerLibrary.address,
            },
        },
        sf.superTokenFactory,
        sf.resolver
    );
    // transfer ownership of governance to super token deployer to allow it to initialize NFT contracts
    await frameworkDeployer.transferOwnership(superTokenDeployer.address);

    // add super token deployer as an admin for the resolver
    const testResolver = await ethers.getContractAt(
        TestResolver.abi,
        sf.resolver
    );
    await testResolver.addAdmin(superTokenDeployer.address);

    return {frameworkDeployer, superTokenDeployer};
};

module.exports = {
    deployTestFramework,
};
