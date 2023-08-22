/**
 * This truffle-config is only used for testing the js-sdk.
 *
 * See the @superfluid-finance/ethereum-contracts package for a good example.
 **/
const path = require("path");
module.exports = {
    // use pre-built artifacts from ethereum-contracts
    contracts_build_directory: path.join(
        __dirname,
        "../ethereum-contracts/build/truffle"
    ),
    // Configure your compilers
    compilers: {
        solc: {
            version: "0.8.16", // Fetch exact version from solc-bin (default: truffle's version)
            settings: {
                // See the solidity docs for advice about optimization and evmVersion
                optimizer: {
                    enabled: true,
                    runs: 200,
                },
                // evmVersion: use default
            },
        },
    },
    networks: {
        /// For truffle development environment
        development: {
            host: "127.0.0.1",
            network_id: "4447",
            port: 47545,

            // workaround to improve testing speed
            // see https://github.com/trufflesuite/truffle/issues/3522
            disableConfirmationListener: true,
        },
    },
};
