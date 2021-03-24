/**
 * This truffle-config is only used for testing the subgraph.
 *
 * See the @superfluid-finance/ethereum-contracts package for a good example.
 **/
const path = require("path");
module.exports = {
    contracts_build_directory: path.join(
        __dirname,
        "../ethereum-contracts/build/contracts"
    ),
    compilers: {
        solc: {
            version: "0.7.6",
            settings: {
                optimizer: {
                    enabled: true,
                    runs: 200,
                },
            },
        },
    },
    networks: {
        ganache: {
            host: "0.0.0.0",
            network_id: "*",
            port: 8545,
            networkCheckTimeout: 10000,
        },
    },
};
