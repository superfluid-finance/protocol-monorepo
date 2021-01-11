/**
 * More information about configuration can be found at:
 *
 * truffleframework.com/docs/advanced/configuration
 **/
const path = require("path");
module.exports = {
    contracts_build_directory: path.join(
        __dirname,
        "../ethereum-contracts/build/contracts"
    ),
    migrations_directory: path.join(
        __dirname,
        "../ethereum-contracts/src/migrations"
    )
};
