const {ethers} = require("hardhat");

const {
    deployTestFramework,
} = require("@superfluid-finance/ethereum-contracts/dev-scripts/deploy-test-framework");

async function deployContractsAndToken() {
    const [adminSigner] = await ethers.getSigners();

    const {frameworkDeployer: deployer, superTokenDeployer} = await deployTestFramework();

    console.log("Deploying Wrapper Super Token...");
    await superTokenDeployer
        .connect(adminSigner)
        .deployWrapperSuperToken(
            "Fake DAI",
            "fDAI",
            18,
            ethers.utils.parseUnits("1000000000000")
        );

    console.log("Deploying Native Asset Super Token...");
    await superTokenDeployer
        .connect(adminSigner)
        .deployNativeAssetSuperToken("Super ETH", "ETHx");

    console.log("Deploying Pure Super Token...");
    await superTokenDeployer
        .connect(adminSigner)
        .deployPureSuperToken(
            "Mr.Token",
            "MRx",
            ethers.utils.parseUnits("1000000000000")
        );
    return deployer;
}

module.exports = {
    deployContractsAndToken,
};
