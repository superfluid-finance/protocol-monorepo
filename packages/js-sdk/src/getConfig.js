/* eslint-disable no-global-assign */
const sfMetadata = require("@superfluid-finance/metadata");

/* istanbul ignore next */
if (typeof module === "undefined") module = {};

const getConfigData = (chainId) => {
    const networkData = sfMetadata.getNetworkByChainId(chainId);
    if (!networkData) {
        console.log(`no metadata found for network with chainId ${chainId}`);
        return {
            nativeTokenSymbol: "",
            resolverAddress: "",
            versions: {
                v1: {
                    subgraphQueryEndpoint: "",
                },
            },
        };
    }

    return {
        nativeTokenSymbol: networkData.nativeTokenSymbol,
        resolverAddress: networkData.contractsV1.resolver,
        versions: {
            v1: {
                subgraphQueryEndpoint: networkData.subgraphV1.hostedEndpoint,
            },
        },
    };
};

let Superfluid_getConfig;
// eslint-disable-next-line no-unused-vars
Superfluid_getConfig = module.exports = function getConfig(chainId, version) {
    const DEFAULT_CONFIGS = {
        //
        // Local testing
        //
        1337: {
            // for default ganache setup
            nativeTokenSymbol: "ETH",
        },
        4447: {
            // for local testing (truffle internal ganache and TestEnvironment)
            nativeTokenSymbol: "ETH",
        },
        5777: {
            // for local testing (external ganache)
            nativeTokenSymbol: "ETH",
        },
        31337: {
            // for local testing hardhat
            nativeTokenSymbol: "ETH",
        },

        //
        // ETHEREUM
        //
        // mainnet
        1: getConfigData(1),
        // goerli
        5: getConfigData(5),
        // sepolia
        11155111: getConfigData(11155111),

        //
        // MATIC: https://docs.matic.network/docs/develop/network-details/network/
        //
        // (matic) mainnet
        137: getConfigData(137),
        // (matic) mumbai testnet
        80001: getConfigData(80001),
        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        0x64: getConfigData(0x64),

        //
        // Optimistic Ethereum: https://community.optimism.io/docs/
        //
        // optimism mainnet
        10: getConfigData(10),
        // optimism goerli
        420: getConfigData(420),

        //
        // Arbitrum: https://developer.offchainlabs.com
        //
        // arbitrum one mainnet
        42161: getConfigData(42161),
        // arbitrum goerli testnet
        421613: getConfigData(421613),

        //
        // Avalanche C-Chain: https://docs.avax.network/learn/platform-overview#contract-chain-c-chain
        //
        // avalanche c-chain mainnet
        43114: getConfigData(43114),
        // avalanche c-chain fuji testnet
        43113: getConfigData(43113),

        //
        // Binance Smart Chain (BSC): https://docs.binance.org/
        //
        56: getConfigData(56),

        //
        // Celo: https://github.com/celo-org/celo-monorepo#docs
        //
        42220: getConfigData(42220),

        //
        // Base: https://base.org/
        //
        // base goerli testnet
        84531: getConfigData(84531),

        //
        // currently unsupported networks
        //
        69: {
            // optimism kovan testnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0x218B65780615Ff134f9Ad810CB98839534D3C0D6",
        },

        42162: {
            // arbitrum rinkeby testnet
            nativeTokenSymbol: "ETH",
            resolverAddress: "0xa2C0C70A1E922f5f060ec20EE3aF002C163b4567",
        },

        97: {
            // BSC chapel testnet
            nativeTokenSymbol: "BNB",
        },

        // ARTIS
        0x03c401: {
            // (artis) tau1 testnet
            resolverAddress: "0x79D426CD219eDCFEB2dCbcf7ea0F8B3642C56F47",
        },

        // Celo
        44787: {
            // celo alfajores testnet
            nativeTokenSymbol: "CELO",
        },
    };

    let configs = {
        ...DEFAULT_CONFIGS[chainId],
    };
    // load version specific configs
    if (configs.versions) {
        configs = {
            ...configs,
            ...configs.versions[version],
        };
        delete configs.versions;
    }
    // overriding environment variables
    if (global && global.process && global.process.env.RESOLVER_ADDRESS) {
        configs.resolverAddress = global.process.env.RESOLVER_ADDRESS;
    }

    return configs;
};
