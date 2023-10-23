const {ethers} = require("hardhat");

const SuperfluidGovDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidGovDeployerLibrary.json");
const SuperfluidHostDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidHostDeployerLibrary.json");
const SuperfluidCFAv1DeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidCFAv1DeployerLibrary.json");
const SuperfluidIDAv1DeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidIDAv1DeployerLibrary.json");
const SuperTokenDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperTokenDeployerLibrary.json");
const SuperfluidPeripheryDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidPeripheryDeployerLibrary.json");
const SuperfluidNFTLogicDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidNFTLogicDeployerLibrary.json");
const ProxyDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/ProxyDeployerLibrary.json");
const CFAv1ForwarderDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/CFAv1ForwarderDeployerLibrary.json");
const IDAv1ForwarderDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/IDAv1ForwarderDeployerLibrary.json");
const SuperfluidLoaderDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/SuperfluidLoaderDeployerLibrary.json");
const SuperfluidFrameworkDeployerArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeployer.sol/SuperfluidFrameworkDeployer.json");
const SlotsBitmapLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/libs/SlotsBitmapLibrary.sol/SlotsBitmapLibrary.json");
const TokenDeployerLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/utils/SuperfluidFrameworkDeploymentSteps.sol/TokenDeployerLibrary.json");

const ERC1820Registry = require("../dev-scripts/artifacts/ERC1820Registry.json");

const ERC1820_ADDRESS = "0x1820a4b7618bde71dce8cdc73aab6c95905fad24";
const ERC1820_BIN = ERC1820Registry.bin;
const ERC1820_DEPLOYER = "0xa990077c3205cbDf861e17Fa532eeB069cE9fF96";
const ERC1820_PAYLOAD =
    "0xf90a388085174876e800830c35008080b909e5" +
    ERC1820_BIN +
    "1ba01820182018201820182018201820182018201820182018201820182018201820a01820182018201820182018201820182018201820182018201820182018201820";

async function deployERC1820(provider) {
    if (process.env.DEBUG_CONSOLE === true) {
        console.log("Deploying ERC1820...");
    }
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

        if (process.env.DEBUG_CONSOLE === true) {
            console.log("ERC1820 registry successfully deployed");
        }
    }
}

/**
 * Gets the address of the deployed contract.
 * This is for handling the different contract objects in ethers v5 (contract.address)
 * vs ethers v6 (contract.target), that is, v6 does not have contract.address and vice versa.
 * @param {ethers.Contract} contract
 * @returns
 */
const getContractAddress = (contract) => {
    return contract.address || contract.target;
};

const _getFactoryAndReturnDeployedContract = async (
    contractName,
    artifact,
    signerOrOptions,
    ...args
) => {
    if (process.env.DEBUG_CONSOLE === true) {
        console.log(`Deploying ${contractName}...`);
    }
    const ContractFactory = await ethers.getContractFactoryFromArtifact(
        artifact,
        signerOrOptions
    );
    const contract = await ContractFactory.deploy(...args);

    // ethers v5
    if (contract.deployed) {
        await contract.deployed();
    } else if (!contract.deployed) {
        // ethers v6
        await contract.waitForDeployment();
    }

    if (process.env.DEBUG_CONSOLE === true) {
        console.log(
            `${contractName} Deployed At:`,
            getContractAddress(contract)
        );
    }
    return contract;
};

/**
 * Deploys Superfluid Framework in local testing environments.
 * 
 * NOTE: This only works with Hardhat + ethers v5/ethers v6 currently.
 * 
 * You must pass either a `privateKey` string OR an `ethersV5Signer` object
 * @param provider an ethers provider
 * @param {string} [privateKey] NEVER USE A PRIVATE KEY WITH REAL FUNDS - a test account private key
 * @param {ethers.providers.Provider} [ethersV5Signer] an ethers v5 signer
 * @returns
 */
const deployTestFramework = async (provider, privateKey, ethersV5Signer) => {
    // use a passed signer OR create one on the spot
    const signer = ethersV5Signer || new ethers.Wallet(privateKey, provider);
    await deployERC1820(provider);
    const SlotsBitmapLibrary = await _getFactoryAndReturnDeployedContract(
        "SlotsBitmapLibrary",
        SlotsBitmapLibraryArtifact,
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
                    SlotsBitmapLibrary: getContractAddress(SlotsBitmapLibrary),
                },
            }
        );

    const SuperTokenDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperTokenDeployerLibrary",
            SuperTokenDeployerLibraryArtifact,
            {
                signer,
            }
        );
    const SuperfluidPeripheryDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidPeripheryDeployerLibrary",
            SuperfluidPeripheryDeployerLibraryArtifact,
            signer
        );
    const SuperfluidNFTLogicDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidNFTLogicDeployerLibrary",
            SuperfluidNFTLogicDeployerLibraryArtifact,
            signer
        );
    const ProxyDeployerLibrary = await _getFactoryAndReturnDeployedContract(
        "ProxyDeployerLibrary",
        ProxyDeployerLibraryArtifact,
        signer
    );
    const CFAv1ForwarderDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "CFAv1ForwarderDeployerLibrary",
            CFAv1ForwarderDeployerLibraryArtifact,
            signer
        );
    const IDAv1ForwarderDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "IDAv1ForwarderDeployerLibrary",
            IDAv1ForwarderDeployerLibraryArtifact,
            signer
        );
    const SuperfluidLoaderDeployerLibrary =
        await _getFactoryAndReturnDeployedContract(
            "SuperfluidLoaderDeployerLibrary",
            SuperfluidLoaderDeployerLibraryArtifact,
            signer
        );
    const TokenDeployerLibrary = await _getFactoryAndReturnDeployedContract(
        "TokenDeployerLibrary",
        TokenDeployerLibraryArtifact,
        signer
    );

    const sfDeployer = await _getFactoryAndReturnDeployedContract(
        "SuperfluidFrameworkDeployer",
        SuperfluidFrameworkDeployerArtifact,
        {
            signer,
            libraries: {
                SuperfluidGovDeployerLibrary: getContractAddress(
                    SuperfluidGovDeployerLibrary
                ),
                SuperfluidHostDeployerLibrary: getContractAddress(
                    SuperfluidHostDeployerLibrary
                ),
                SuperfluidCFAv1DeployerLibrary: getContractAddress(
                    SuperfluidCFAv1DeployerLibrary
                ),
                SuperfluidIDAv1DeployerLibrary: getContractAddress(
                    SuperfluidIDAv1DeployerLibrary
                ),
                SuperfluidPeripheryDeployerLibrary: getContractAddress(
                    SuperfluidPeripheryDeployerLibrary
                ),
                SuperTokenDeployerLibrary: getContractAddress(
                    SuperTokenDeployerLibrary
                ),
                SuperfluidNFTLogicDeployerLibrary: getContractAddress(
                    SuperfluidNFTLogicDeployerLibrary
                ),
                ProxyDeployerLibrary: getContractAddress(ProxyDeployerLibrary),
                CFAv1ForwarderDeployerLibrary: getContractAddress(
                    CFAv1ForwarderDeployerLibrary
                ),
                IDAv1ForwarderDeployerLibrary: getContractAddress(
                    IDAv1ForwarderDeployerLibrary
                ),
                SuperfluidLoaderDeployerLibrary: getContractAddress(
                    SuperfluidLoaderDeployerLibrary
                ),
                TokenDeployerLibrary: getContractAddress(TokenDeployerLibrary),
            },
        }
    );
    const numSteps = await sfDeployer.getNumSteps();
    for (let i = 0; i < numSteps; i++) {
        await sfDeployer.executeStep(i);
    }
    return {frameworkDeployer: sfDeployer};
};

module.exports = {
    deployTestFramework,
};
