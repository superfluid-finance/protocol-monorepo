const {ethers } = require("hardhat");

const SlotsBitmapLibraryArtifact = require("@superfluid-finance/ethereum-contracts/build/contracts/SlotsBitmapLibrary.json");
const SuperfluidFrameworkDeployerArtifact = require("@superfluid-finance/ethereum-contracts/build/contracts/SuperfluidFrameworkDeployer.json");

const ERC1820Registry = require("@superfluid-finance/ethereum-contracts/scripts/artifacts/ERC1820Registry.json");

const ERC1820_ADDRESS = "0x1820a4b7618bde71dce8cdc73aab6c95905fad24";
const ERC1820_DEPLOYER = "0xa990077c3205cbDf861e17Fa532eeB069cE9fF96";
const ERC1820_BIN = ERC1820Registry.bin;
const ERC1820_PAYLOAD =
    "0xf90a388085174876e800830c35008080b909e5" +
    ERC1820_BIN +
    "1ba01820182018201820182018201820182018201820182018201820182018201820a01820182018201820182018201820182018201820182018201820182018201820";

//USAGE
//this script will deploy an instance of the SuperfluidFrameworkDeployer contract so that it may be used within testing environments
//1) users will import this file from the @superfluid-finance/ethereum-contracts npm package
//2) the contract will be deployed via this script
//3) users can then call getFramework(), deployWrapperToken(), etc on this contract

//NOTE: the ERC1820 registry must be deployed first before the framework can be deployed
async function deployERC1820(provider) {
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

const deployTestFramework = async () => {
    const signer = (await ethers.getSigners())[0];
    await deployERC1820(ethers.provider);

    //NOTE: the framework deployer requires the SlotsBitmapLibrary 
    const SlotsBitmapLibrary = await new ethers.ContractFactory(
        SlotsBitmapLibraryArtifact.abi,
        SlotsBitmapLibraryArtifact.bytecode,
        signer
    ).deploy();

    const slotsBitmapLibraryPlaceholder = "__SlotsBitmapLibrary____________________"
    const addr = SlotsBitmapLibrary.address.replace("0x", "")
        
    console.log("Deploying the deployer...")
    const superfluidFrameworkDeployer = await new ethers.ContractFactory(
        SuperfluidFrameworkDeployerArtifact.abi,
        SuperfluidFrameworkDeployerArtifact.bytecode
            .split(slotsBitmapLibraryPlaceholder)
            .join(addr),
        signer
    ).deploy({ gasLimit: 100000000})
    console.log("Deployer deployed successfully");
    return superfluidFrameworkDeployer;
};

module.exports = deployTestFramework;
