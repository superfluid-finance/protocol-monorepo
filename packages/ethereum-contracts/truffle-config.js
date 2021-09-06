/**
 * Use this file to configure your truffle project. It's seeded with some
 * common settings for different networks and features like migrations,
 * compilation and testing. Uncomment the ones you need or modify
 * them to suit your project as necessary.
 *
 * More information about configuration can be found at:
 *
 * truffleframework.com/docs/advanced/configuration
 *
 * To deploy via Infura you'll need a wallet provider (like truffle-hdwallet-provider)
 * to sign your transactions before they're sent to a remote public node. Infura accounts
 * are available for free at: infura.io/register.
 *
 * You'll also need a mnemonic - the twelve word phrase the wallet uses to generate
 * public/private key pairs. If you're publishing your code to GitHub make sure you load this
 * phrase from a file you've .gitignored so it doesn't accidentally become public.
 *
 */

const HDWalletProvider = require("@truffle/hdwallet-provider");
try {
    require("dotenv").config();
} catch (error) {
    console.error(
        "Loading .env file failed. Things will likely fail. You may want to copy .env.template and create a new one."
    );
}

const DEFAULT_NETWORK_TIMEOUT = 60000;

//
// This is a hack to resolve that HDWallet doesn't work with openethereum (xdai & kovan)
//
// Related issues:
// - https://github.com/trufflesuite/truffle/issues/3182
// - https://github.com/openethereum/parity-ethereum/issues/11824
// - https://github.com/MetaMask/web3-provider-engine/issues/311
function createProviderForOpenEthereum(url) {
    let provider;
    const Web3WsProvider = require("web3-providers-ws");
    if (url.startsWith("ws:") || url.startsWith("wss:")) {
        provider = new Web3WsProvider(url);
        // apply the skipCache hack
        const origSend = provider.__proto__.send;
        provider.__proto__.send = function (payload, callback) {
            delete payload.skipCache;
            origSend.call(provider, payload, callback);
        };
    } else {
        // let hdwallet provider handle the url directly
        provider = url;
    }
    return provider;
}

module.exports = {
    plugins: [
        //"truffle-security",
        "solidity-coverage",
        "truffle-plugin-verify",
    ],
    /**
     * Networks define how you connect to your ethereum client and let you set the
     * defaults web3 uses to send transactions. If you don't specify one truffle
     * will spin up a development blockchain for you on port 9545 when you
     * run `develop` or `test`. You can ask a truffle command to use a specific
     * network from the command line, e.g
     *
     * $ truffle test --network <network-name>
     */

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
            network_id: 4,
            //gas: 8e6,
            gasPrice: 10e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
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
            network_id: 3,
            //gas: 7.9e6,
            gasPrice: 10e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
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
            network_id: 5,
            //gas: 8e6,
            gasPrice: +process.env.GOERLI_GAS_PRICE || 10e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        kovan: {
            provider: () => {
                return new HDWalletProvider(
                    process.env.KOVAN_MNEMONIC,
                    createProviderForOpenEthereum(
                        process.env.KOVAN_PROVIDER_URL
                    ),
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 42,
            //gas: 8e6,
            gasPrice: +process.env.KOVAN_GAS_PRICE || 10e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        arbitrum: {
            provider: function () {
                return new HDWalletProvider(
                    process.env.ARBITRUM_MNEMONIC,
                    process.env.ARBITRUM_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: "*",
            //gas: 1e9, // arbgas is a different beast, 1G gas is normal
            gasPrice: 0,
        },

        xdai: {
            provider: () => {
                return new HDWalletProvider(
                    process.env.XDAI_MNEMONIC,
                    createProviderForOpenEthereum(
                        process.env.XDAI_PROVIDER_URL
                    ),
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 0x64,
            //gas: 8e6,
            gasPrice: +process.env.XDAI_GAS_PRICE || 10e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        matic: {
            provider: () => {
                return new HDWalletProvider(
                    process.env.MATIC_MNEMONIC,
                    createProviderForOpenEthereum(
                        process.env.MATIC_PROVIDER_URL
                    ),
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 137,
            //gas: 8e6,
            gasPrice: +process.env.MATIC_GAS_PRICE || 25e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
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
            network_id: 80001,
            //gas: 8e6,
            gasPrice: +process.env.MUMBAI_GAS_PRICE || 20e9,
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
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
            //gas: 8e6,
            gasPrice: +process.env.ARTIS_GAS_PRICE || 1e9, // default 1 gwei
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
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
            //gas: 8e6,
            gasPrice: +process.env.MAINNET_GAS_PRICE || 1e9, // default 1 gwei
            //confirmations: 6, // # of confs to wait between deployments. (default: 0)
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        coverage: {
            host: "localhost",
            network_id: "*",
            port: 8555, // <-- If you change this, also set the port option in .solcover.js.
            gas: 0xfffffffffff, // <-- Use this high gas value
            gasPrice: 0x01, // <-- Use this low gas price
        },

        ganache: {
            host: "127.0.0.1",
            network_id: "*",
            port: process.env.GANACHE_PORT || 8545,
        },

        // Another network with more advanced options...
        // advanced: {
        // port: 8777,             // Custom port
        // network_id: 1342,       // Custom network
        // gas: 8500000,           // Gas sent with each transaction (default: ~6700000)
        // gasPrice: 20000000000,  // 20 gwei (in wei) (default: 100 gwei)
        // from: <address>,        // Account to send txs from (default: accounts[0])
        // websockets: true        // Enable EventEmitter interface for web3 (default: false)
        // },

        // Useful for deploying to a public network.
        // Note: It's important to wrap the provider as a function.
        // ropsten: {
        // provider: () => new HDWalletProvider(mnemonic, `https://ropsten.infura.io/v3/YOUR-PROJECT-ID`),
        // network_id: 3,       // Ropsten's id
        // gas: 5500000,        // Ropsten has a lower block limit than mainnet
        // confirmations: 2,    // # of confs to wait between deployments. (default: 0)
        // timeoutBlocks: 200,  // # of blocks before a deployment times out  (minimum/default: 50)
        // skipDryRun: true     // Skip dry run before migrations? (default: false for public nets )
        // },

        // Useful for private networks
        // private: {
        // provider: () => new HDWalletProvider(mnemonic, `https://network.io`),
        // network_id: 2111,   // This network is yours, in the cloud.
        // production: true    // Treats this network as if it was a public net. (default: false)
        // }
    },

    // Set default mocha options here, use special reporters etc.
    //mocha: {
    //    // timeout: 100000
    //    reporter: "eth-gas-reporter",
    //    reporterOptions: {
    //        noColors: true,
    //        outputFile: "build/eth-gas-report.txt"
    //    }
    //},

    // Configure your compilers
    compilers: {
        solc: {
            version: "0.7.6", // Fetch exact version from solc-bin (default: truffle's version)
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

    api_keys: {
        etherscan: process.env.ETHERSCAN_API_KEY,
        polygonscan: process.env.POLYGONSCAN_API_KEY,
    },
};
