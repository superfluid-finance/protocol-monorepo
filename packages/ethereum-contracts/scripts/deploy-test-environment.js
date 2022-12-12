const boilerplate = require("./boilerplate");

module.exports = function () {
    boilerplate(
        "deploy-test-environment.js",
        "IF you have already completed the migration for scripts/deploy-test-token.js, you can just delete the import for scripts/deploy-test-token.js"
    );
    console.log(
        "If you are using Hardhat, below is how to migrate from deploy-test-environment.js to deploy-test-framework.js:"
    );
    console.log(`
    // previous deploy framework 
    const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework");
    // or 
    // import deployFramework from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework";

    await deployFramework((x: any) => errorHandler("Framework", x), {
        web3: (global as any).web3,
        from: Deployer,
    });

    // previous deploy framework end


    // new deploy framework

    const { deployTestFramework } = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework");
    // or 
    // import { deployTestFramework } from "@superfluid-finance/ethereum-contracts/scripts/deploy-test-framework";

    const deployer = await deployTestFramework(); // this returns the SuperfluidFrameworkDeployer 

    // new deploy framework end\n\n`);

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
