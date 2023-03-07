const {ethers} = require("hardhat");
const testResolverArtifact = require("@superfluid-finance/ethereum-contracts/artifacts/contracts/utils/TestResolver.sol/TestResolver.json");

const {
    deployTestFramework,
} = require("@superfluid-finance/ethereum-contracts/dev-scripts/deploy-test-framework");

async function deployContractsAndToken() {
    const [Deployer] = await ethers.getSigners();

    const deployer = await deployTestFramework();
    const framework = await deployer.getFramework();

    const resolver = await ethers.getContractAt(
        testResolverArtifact.abi,
        framework.resolver
    );

    await deployer
        .connect(Deployer)
        .deployWrapperSuperToken(
            "Fake DAI",
            "fDAI",
            18,
            ethers.utils.parseUnits("1000000000000")
        );

    await deployer
        .connect(Deployer)
        .deployNativeAssetSuperToken("Super ETH", "ETHx");

    await deployer
        .connect(Deployer)
        .deployPureSuperToken(
            "Mr.Token",
            "MRx",
            ethers.utils.parseUnits("1000000000000")
        );

    const fDAIAddress = await resolver.get("tokens.test.fDAI");
    const fDAIxAddress = await resolver.get("supertokens.test.fDAIx");
    const ethxAddress = await resolver.get("supertokens.test.ETHx");
    const mrxAddress = await resolver.get("supertokens.test.MRx");
    const tokenDeploymentOutput = {
        wrapperSuperTokenData: {
            wrapperSuperTokenName: "Super Fake DAI",
            wrapperSuperTokenSymbol: "fDAIx",
            wrapperSuperTokenAddress: fDAIxAddress,
            wrapperSuperTokenUnderlyingToken: {
                underlyingTokenAddress: fDAIAddress,
                underlyingTokenSymbol: "fDAI",
                underlyingTokenName: "Fake DAI",
            },
        },
        nativeAssetSuperTokenData: {
            name: "Super ETH",
            symbol: "ETHx",
            nativeAssetSuperTokenAddress: ethxAddress,
        },
        pureSuperTokenData: {
            name: "Mr.Token",
            symbol: "MRx",
            pureSuperTokenAddress: mrxAddress,
        },
    };
    console.log("Token Deployment Output:", tokenDeploymentOutput);

    return {deployer, tokenDeploymentOutput};
}

module.exports = {
    deployContractsAndToken,
};
