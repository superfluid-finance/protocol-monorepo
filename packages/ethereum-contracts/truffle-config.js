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

const E = (module.exports = {
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

        //
        // ETHEREUM: https://ethereum.org/
        //
        "eth-mainnet": {
            provider: () =>
                new HDWalletProvider(
                    process.env.MAINNET_MNEMONIC,
                    process.env.MAINNET_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 1, // mainnet's id
            gasPrice: +process.env.MAINNET_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "eth-rinkeby": {
            provider: () =>
                new HDWalletProvider(
                    process.env.RINKEBY_MNEMONIC,
                    process.env.RINKEBY_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 4,
            gasPrice: +process.env.RINKEBY_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "eth-ropsten": {
            provider: () =>
                new HDWalletProvider(
                    process.env.ROPSTEN_MNEMONIC,
                    process.env.ROPSTEN_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 3,
            gasPrice: +process.env.ROPSTEN_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "eth-goerli": {
            provider: () =>
                new HDWalletProvider(
                    process.env.GOERLI_MNEMONIC,
                    process.env.GOERLI_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 5,
            gasPrice: +process.env.GOERLI_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "eth-kovan": {
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
            gasPrice: +process.env.KOVAN_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Polygon: https://docs.polygon.technology/docs/develop/network-details/network/
        //
        "polygon-mainnet": {
            provider: () => {
                return new HDWalletProvider(
                    process.env.MATIC_MNEMONIC,
                    process.env.MATIC_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 137,
            gasPrice: +process.env.MATIC_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "polygon-mumbai": {
            provider: () =>
                new HDWalletProvider(
                    process.env.MUMBAI_MNEMONIC,
                    process.env.MUMBAI_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 80001,
            gasPrice: +process.env.MUMBAI_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        "xdai-mainnet": {
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
            network_id: 100,
            gasPrice: +process.env.XDAI_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Optimistic Ethereum: https://community.optimism.io/docs/
        //
        "optimism-mainnet": {
            provider: function () {
                return new HDWalletProvider(
                    process.env.OPMAINNET_MNEMONIC,
                    process.env.OPMAINNET_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 10,
            gasPrice: +process.env.OPMAINNET_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "optimism-kovan": {
            provider: function () {
                return new HDWalletProvider(
                    process.env.OPKOVAN_MNEMONIC,
                    process.env.OPKOVAN_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 69,
            gasPrice: +process.env.OPKOVAN_GAS_PRICE,
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Arbitrum: https://developer.offchainlabs.com
        //
        "arbitrum-one": {
            provider: function () {
                return new HDWalletProvider(
                    process.env.ARBONE_MNEMONIC,
                    process.env.ARBONE_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 42161,
            gasPrice: +process.env.ARBONE_GAS_PRICE,
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "arbitrum-rinkeby": {
            provider: function () {
                return new HDWalletProvider(
                    process.env.ARBRINKEBY_MNEMONIC,
                    process.env.ARBRINKEBY_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 421611,
            gasPrice: +process.env.ARBRINKEBY_GAS_PRICE,
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Avalanche C-Chain: https://docs.avax.network/learn/platform-overview#contract-chain-c-chain
        //
        "avalanche-C": {
            provider: () =>
                new HDWalletProvider(
                    process.env.AVALANCHE_MNEMONIC,
                    process.env.AVALANCHE_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true, // shareNonce
                    "m/44'/60'/0'/0/", // needed bcs we want to add chainId
                    43114 // chainId
                ),
            network_id: 1,
            gasPrice: +process.env.AVALANCHE_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "avalanche-fuji": {
            provider: () =>
                new HDWalletProvider(
                    process.env.AVAFUJI_MNEMONIC,
                    process.env.AVAFUJI_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true, // shareNonce
                    "m/44'/60'/0'/0/", // needed bcs we want to add chainId
                    43113 // chainId
                ),
            network_id: 1,
            gasPrice: +process.env.AVAFUJI_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Binance Smart Chain: https://docs.binance.org/smart-chain/developer/rpc.html
        //
        "bsc-mainnet": {
            provider: () =>
                new HDWalletProvider(
                    process.env.BSC_MNEMONIC,
                    process.env.BSC_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                ),
            network_id: 56,
            gasPrice: +process.env.BSC_GAS_PRICE,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Celo: https://docs.celo.org/getting-started/choosing-a-network
        //
        "celo-mainnet": {
            provider: function () {
                return new HDWalletProvider(
                    process.env.CELO_MNEMONIC,
                    process.env.CELO_PROVIDER_URL,
                    0, //address_index
                    10, // num_addresses
                    true // shareNonce
                );
            },
            network_id: 42220,
            gasPrice: +process.env.CELO_GAS_PRICE,
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        /// For truffle development environment
        development: {
            host: "localhost",
            port: 47545,
            network_id: "4447",

            // workaround to improve testing speed
            // see https://github.com/trufflesuite/truffle/issues/3522
            disableConfirmationListener: true,
        },

        coverage: {
            host: "localhost",
            port: 8555, // <-- If you change this, also set the port option in .solcover.js.

            // ditto
            disableConfirmationListener: true,
        },

        /// For other private test environment
        private: {
            host: "localhost",
            port: process.env.PRIVATE_PROVIDER_PORT || 8545,
            network_id: "*",

            // ditto
            disableConfirmationListener: true,
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
});

//
// Creating network aliases
//

// The OGs
E.networks.ropsten = E.networks["eth-ropsten"];
E.networks.rinkeby = E.networks["eth-rinkeby"];
E.networks.kovan = E.networks["eth-kovan"];
E.networks.goerli = E.networks["eth-goerli"];
E.networks.xdai = E.networks["xdai-mainnet"];
E.networks.matic = E.networks["polygon-mainnet"];
E.networks.mumbai = E.networks["polygon-mumbai"];

// Other aliases
E.networks.opmainnet = E.networks["optimism-mainnet"];
E.networks.opkovan = E.networks["optimism-kovan"];
E.networks.arbone = E.networks["arbitrum-one"];
E.networks.arbrinkeby = E.networks["arbitrum-rinkeby"];
E.networks.avalanche = E.networks["avalanche-C"];
E.networks.avafuji = E.networks["avalanche-fuji"];
E.networks.bsc = E.networks["bsc-mainnet"];
E.networks.celo = E.networks["celo-mainnet"];
