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
        "../ethereum-contracts/build/contracts"
    ),
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
