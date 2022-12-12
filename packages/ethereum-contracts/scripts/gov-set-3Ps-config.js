const boilerplate = require("./boilerplate");

module.exports = function () {
    boilerplate(
        "gov-set-3Ps-config.js",
        "IF you have already completed the migration for any of the token deployments, you can just delete the import for scripts/gov-set-3Ps-config.js\n"
    );

    console.log(
        "The SuperfluidFrameworkDeployer contract sets default values for 3Ps configs.\n"
    );

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
