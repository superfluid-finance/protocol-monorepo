const HDWalletProvider = require("@truffle/hdwallet-provider");
require("dotenv").config();
const GAS_LIMIT = 8000000;

module.exports = {
    networks: {
        goerli: {
            provider: () =>
                new HDWalletProvider(
                    process.env.GOERLI_MNEMONIC,
                    process.env.GOERLI_PROVIDER_URL
                ),
            network_id: 5, // Goerli's id
            gas: GAS_LIMIT,
            gasPrice: 11e9, // 10 GWEI
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false // Skip dry run before migrations? (default: false for public nets )
        }
    },
    compilers: {
        solc: {
            version: "0.7.6" // Fetch exact version from solc-bin (default: truffle's version)
        }
    }
};
