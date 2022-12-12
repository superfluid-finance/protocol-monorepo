const boilerplate = require("./boilerplate");

module.exports = function () {
    boilerplate("deploy-unlisted-pure-super-token.js");
    console.log(
        "If you are using Hardhat, below is how to migrate from deploy-unlisted-pure-super-token.js to deploy-test-framework.js:"
    );
    console.log(`
    // previous deploy pure super token
    const deployPureSuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-unlisted-pure-super-token");
    // or
    // import deployPureSuperToken from "@superfluid-finance/ethereum-contracts/scripts/deploy-unlisted-pure-super-token";
    const mrTokenAddress = await deployPureSuperToken(
        (x: any) => errorHandler("NativeSuperToken", x),
        [":", "Mr.Token", "MRx", "10000000"],
        {
            web3: (global as any).web3,
            from: Deployer,
        }
    );
    // previous deploy pure super token end


    // new deploy pure super token 

    const { deployTestFramework } = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework");
    // or 
    // import { deployTestFramework } from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework";

    const deployer = await deployTestFramework(); // this returns the SuperfluidFrameworkDeployer 

    // this deploys a pure super token
    await deployer
        .connect(Deployer)
        .deployPureSuperToken(
            "Mr.Token",                                 // pure super token name
            "MRx",                                      // pure super token symbol
            ethers.utils.parseUnits("1000000000000")    // pure super token initial supply
        );
    // new deploy pure super token end
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
