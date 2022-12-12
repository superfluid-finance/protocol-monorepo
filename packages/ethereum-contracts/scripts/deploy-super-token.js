const boilerplate = require("./boilerplate");

module.exports = function () {
    boilerplate(
        "deploy-super-token.js",
        "IF you have already completed the migration for scripts/deploy-test-token.js, you can just delete the import for scripts/deploy-test-token.js"
    );
    console.log(
        "If you are using Hardhat, below is how to migrate from deploy-super-token.js to deploy-test-framework.js:"
    );
    console.log(`
    // FOR WRAPPER SUPER TOKENS
    // previous deploy wrapper super token 
    const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
    // or
    // import deploySuperToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-super-token";
    await deploySuperToken(
        (x: any) => errorHandler("SuperToken", x),
        [":", "fDAI"],
        {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
    // previous deploy wrapper super token end

    // new deploy wrapper erc20 token 

    const { deployTestFramework } = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework");
    // or 
    // import { deployTestFramework } from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework";

    const deployer = await deployTestFramework(); // this returns the SuperfluidFrameworkDeployer 

    // this deploys an ERC20 Test token and its Wrapper Super Token
    await deployer
        .connect(Deployer)
        .deployWrapperSuperToken(
            "Fake DAI",                                 // underlying token name
            "fDAI",                                     // underlying token symbol
            18,                                         // underlying token decimals
            ethers.utils.parseUnits("1000000000000")    // underlying token total supply
        );
    // new deploy wrapper erc20 token end

    // FOR NATIVE ASSET SUPER TOKENS
    // previous deploy native asset super token
    await deploySuperToken(
        (x: any) => errorHandler("SuperToken", x),
        [":", "ETH"],
        {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
    // previous deploy native asset super token end

    // new deploy native asset super token
    await deployer
        .connect(Deployer)
        .deployNativeAssetSuperToken("Super ETH", "ETHx");
    // new deploy native asset super token end
    `);

    console.log(
        "NOTE: You will need to apply migration changes for all other imports from scripts/*.js\n"
    );
    console.log(
        "Refer to the files in node_modules/@superfluid-finance/ethereum-contracts/scripts/*.js for migration steps for other files.\n\n"
    );
    throw new Error(
        "Please complete the migration, please refer to the other files in /scripts in your node_modules or fix them one at a time."
    );
};
