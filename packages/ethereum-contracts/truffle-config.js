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

//
// This is a hack to resolve that HDWallet doesn't work with openethereum (xdai & kovan)
// via websocket due to an excess parameter ("skipCache")
//
// Related issues:
// - https://github.com/trufflesuite/truffle/issues/3182
// - https://github.com/openethereum/parity-ethereum/issues/11824
// - https://github.com/MetaMask/web3-provider-engine/issues/311
// function createProviderWithOEWorkaround(url) {
//     let provider;
//     const Web3WsProvider = require("web3-providers-ws");
//     if (url.startsWith("ws:") || url.startsWith("wss:")) {
//         provider = new Web3WsProvider(url);
//         // apply the skipCache hack
//         const origSend = provider.__proto__.send;
//         provider.__proto__.send = function (payload, callback) {
//             delete payload.skipCache;
//             origSend.call(provider, payload, callback);
//         };
//     } else {
//         // let hdwallet provider handle the url directly
//         provider = url;
//     }
//     return provider;
// }

const ALIASES = {
    "eth-mainnet": ["mainnet"],
    "eth-goerli": ["goerli"],

    "xdai-mainnet": ["xdai"],

    "polygon-mainnet": ["matic"],
    "polygon-mumbai": ["mumbai"],

    "optimism-mainnet": ["opmainnet"],
    "optimism-goerli": ["opgoerli"],

    "arbitrum-one": ["arbone"],
    "arbitrum-goerli": ["arbgoerli"],

    "avalanche-c": ["avalanche"],
    "avalanche-fuji": ["avafuji"],

    "bsc-mainnet": ["bsc"],

    // currently unsupported
    //
    "optimism-kovan": ["opkovan"],

    "arbitrum-rinkeby": ["arbrinkeby"],

    "bsc-chapel": ["chapel"],

    "celo-mainnet": ["celo"],
    "celo-alfajores": ["alfajores"],
};

const DEFAULT_NETWORK_TIMEOUT = 60000;

/**
 * Get the environment value for the network, supporting network aliases
 *
 * E.g. for MNEMONIC key, these environment keys are tried (high to low priority):
 * ETH_MAINNET_MNEMONIC
 * MAINNET_MNEMONIC
 * DEFAULT_MNEMONIC,
 */
function getEnvValue(networkName, key) {
    const keysToTry = [networkName, ...ALIASES[networkName]]
        .map((i) => i.replace(/-/g, "_").toUpperCase() + "_" + key)
        .concat("DEFAULT_" + key);
    const values = keysToTry.map((i) => process.env[i]).filter((i) => !!i);
    return values[0];
}

/**
 * Create default network configurations
 *
 * NOTE: set providerWrapper to createProviderWithOEWorkaround in case this is still needed,
 *       by default it's an identity function.
 */
function createNetworkDefaultConfiguration(
    networkName,
    providerWrapper = (a) => a
) {
    return {
        provider: () =>
            new HDWalletProvider({
                mnemonic: getEnvValue(networkName, "MNEMONIC"),
                url: providerWrapper(getEnvValue(networkName, "PROVIDER_URL")),
                addressIndex: 0,
                numberOfAddresses: 10,
                shareNonce: true,
            }),
        gasPrice: +getEnvValue(networkName, "GAS_PRICE"),
    };
}

const E = (module.exports = {
    plugins: [
        //"truffle-security",
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
            ...createNetworkDefaultConfiguration("eth-mainnet"),
            network_id: 1, // mainnet's id
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "eth-goerli": {
            ...createNetworkDefaultConfiguration("eth-goerli"),
            network_id: 5,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Polygon: https://docs.polygon.technology/docs/develop/network-details/network/
        //
        "polygon-mainnet": {
            ...createNetworkDefaultConfiguration("polygon-mainnet"),
            network_id: 137,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "polygon-mumbai": {
            ...createNetworkDefaultConfiguration("polygon-mumbai"),
            network_id: 80001,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        "xdai-mainnet": {
            ...createNetworkDefaultConfiguration("xdai-mainnet"),
            network_id: 100,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Optimistic Ethereum: https://community.optimism.io/docs/
        //
        "optimism-mainnet": {
            ...createNetworkDefaultConfiguration("optimism-mainnet"),
            network_id: 10,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "optimism-goerli": {
            ...createNetworkDefaultConfiguration("optimism-goerli"),
            network_id: 420,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Arbitrum: https://developer.offchainlabs.com
        //
        "arbitrum-one": {
            ...createNetworkDefaultConfiguration("arbitrum-one"),
            network_id: 42161,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "arbitrum-goerli": {
            ...createNetworkDefaultConfiguration("arbitrum-goerli"),
            network_id: 421613,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Avalanche C-Chain: https://docs.avax.network/learn/platform-overview#contract-chain-c-chain
        //
        "avalanche-c": {
            ...createNetworkDefaultConfiguration("avalanche-c"),
            network_id: 43114,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "avalanche-fuji": {
            ...createNetworkDefaultConfiguration("avalanche-fuji"),
            network_id: 43113,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Binance Smart Chain: https://docs.binance.org/smart-chain/developer/rpc.html
        //
        "bsc-mainnet": {
            ...createNetworkDefaultConfiguration("bsc-mainnet"),
            network_id: 56,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Currently unsupported networks
        //

        "optimism-kovan": {
            ...createNetworkDefaultConfiguration("optimism-kovan"),
            network_id: 69,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "arbitrum-rinkeby": {
            ...createNetworkDefaultConfiguration("arbitrum-rinkeby"),
            network_id: 421611,
            gas: 250e6, // arbgas is different and estimation fails for expensive txs
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        "bsc-chapel": {
            ...createNetworkDefaultConfiguration("bsc-chapel"),
            network_id: 97,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        //
        // Celo: https://docs.celo.org/getting-started/choosing-a-network
        //
        "celo-mainnet": {
            ...createNetworkDefaultConfiguration("celo-mainnet"),
            network_id: 42220,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },
        "celo-alfajores": {
            ...createNetworkDefaultConfiguration("celo-alfajores"),
            network_id: 44787,
            timeoutBlocks: 50, // # of blocks before a deployment times out  (minimum/default: 50)
            skipDryRun: false, // Skip dry run before migrations? (default: false for public nets )
            networkCheckTimeout: DEFAULT_NETWORK_TIMEOUT,
        },

        /// For truffle development environment
        development: {
            host: "127.0.0.1",
            port: 47545,
            network_id: "4447",

            // workaround to improve testing speed
            // see https://github.com/trufflesuite/truffle/issues/3522
            disableConfirmationListener: true,
        },

        coverage: {
            host: "127.0.0.1",
            port: 8555, // <-- If you change this, also set the port option in .solcover.js.

            // ditto
            disableConfirmationListener: true,
        },

        /// For other private test environment
        private: {
            host: "127.0.0.1",
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

    mocha: {
        // timeout: 100000
        reporter: "mochawesome",
        reporterOptions: {
            json: false,
            reportDir: "test/output/mochareport",
        },
    },

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

    api_keys: {
        etherscan: process.env.ETHERSCAN_API_KEY,
        polygonscan: process.env.POLYGONSCAN_API_KEY,
        snowtrace: process.env.SNOWTRACE_API_KEY,
        optimistic_etherscan: process.env.OPTIMISTIC_API_KEY,
        bscscan: process.env.BSCSCAN_API_KEY,
        arbiscan: process.env.ARBISCAN_API_KEY,
        gnosisscan: process.env.GNOSISSCAN_API_KEY,
    },
});

// Creating network aliases
Object.keys(ALIASES).forEach((networkName) => {
    ALIASES[networkName].forEach((alias) => {
        E.networks[alias] = E.networks[networkName];
    });
});
