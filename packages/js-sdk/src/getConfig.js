/* eslint-disable no-global-assign */
const sfMetadata = require("@superfluid-finance/metadata");

/* istanbul ignore next */
if (typeof module === "undefined") module = {};

const getConfigData = (chainId) => {
    const networkData = sfMetadata.getNetworkByChainId(chainId);
    if (!networkData) {
        console.log(`no metadata found for network with chainId ${chainId}`);
        return {
            isTestnet: true,
            nativeTokenSymbol: "ETH",
            versions: {
                v1: {},
            },
        };
    }

    return {
        isTestnet: networkData.isTestnet,
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
    const CONFIG_OVERRIDES = {
        //
        // currently unsupported networks
        //

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

    const configs = CONFIG_OVERRIDES[chainId] || getConfigData(chainId);

    // overriding environment variables
    if (global && global.process && global.process.env.RESOLVER_ADDRESS) {
        configs.resolverAddress = global.process.env.RESOLVER_ADDRESS;
    }

    return configs;
};
