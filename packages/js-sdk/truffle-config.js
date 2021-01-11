/**
 * More information about configuration can be found at:
 *
 * truffleframework.com/docs/advanced/configuration
 **/
const path = require("path");
const HDWalletProvider = require("@truffle/hdwallet-provider");
require("dotenv").config();
module.exports = {
    contracts_build_directory: path.join(
        __dirname,
        "../ethereum-contracts/build/contracts"
    ),
    migrations_directory: path.join(
        __dirname,
        "../ethereum-contracts/src/migrations"
    ),

    // Set default mocha options here, use special reporters etc.
    //mocha: {
    //    // timeout: 100000
    //    reporter: "eth-gas-reporter",
    //    reporterOptions: {
    //        noColors: true,
    //        outputFile: "build/eth-gas-report.txt"
    //    }
    //},
    networks: {
        // Useful for testing. The `development` name is special - truffle uses it by default
        // if it's defined here and no other network is specified at the command line.
        // You should run a client (like ganache-cli, geth or parity) in a separate terminal
        // tab if you use this network and you must also set the `host`, `port` and `network_id`
        // options below to some value.

        rinkeby: {
            provider: () =>
                new HDWalletProvider(
                    process.env.RINKEBY_MNEMONIC,
                    process.env.RINKEBY_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 4, // Rinkeby's id
            gas: 8e6,
            gasPrice: 10e9, // 10 GWEI
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        ropsten: {
            provider: () =>
                new HDWalletProvider(
                    process.env.ROPSTEN_MNEMONIC,
                    process.env.ROPSTEN_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 3, // Ropsten's id
            gas: 8e6,
            gasPrice: 10e9, // 10 GWEI
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        goerli: {
            provider: () =>
                new HDWalletProvider(
                    process.env.GOERLI_MNEMONIC,
                    process.env.GOERLI_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 5, // Goerli's id
            gas: 8e6,
            gasPrice: +process.env.GOERLI_GAS_PRICE || 100e9, // 100 GWEI, goerli is busy!
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        mainnet: {
            provider: () =>
                new HDWalletProvider(
                    process.env.MAINNET_MNEMONIC,
                    process.env.MAINNET_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 1, // mainnet's id
            gas: 8e6,
            gasPrice: +process.env.MAINNET_GAS_PRICE || 1e9, // default 1 gwei
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        mumbai: {
            provider: () =>
                new HDWalletProvider(
                    process.env.MUMBAI_MNEMONIC,
                    process.env.MUMBAI_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 80001, // matic mumbai id
            gas: 8e6,
            gasPrice: +process.env.MUMBAI_GAS_PRICE || 1e9, // default 1 gwei
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        artis_tau1: {
            provider: () =>
                new HDWalletProvider(
                    process.env.ARTIS_MNEMONIC,
                    process.env.ARTIS_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 0x03c401, // artis tau1 network
            gas: 8e6,
            gasPrice: +process.env.ARTIS_GAS_PRICE || 1e9, // default 1 gwei
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        },

        coverage: {
            host: "localhost",
            network_id: "*",
            port: 8555, // <-- If you change this, also set the port option in .solcover.js.
            gas: 0xfffffffffff, // <-- Use this high gas value
            gasPrice: 0x01 // <-- Use this low gas price
        },

        ganache: {
            host: "127.0.0.1",
            network_id: "*",
            port: 8545
        }
    }
};
