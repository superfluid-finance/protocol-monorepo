/* eslint-disable no-global-assign */
const sfMetadata = require("@superfluid-finance/metadata");

/* istanbul ignore next */
if (typeof module === "undefined") module = {};

function getSubgraphEndpoint(networkName) {
    // use the override endpoint if set
    const overrideEndpointVarName = `${networkName
        .replace(/-/g, "_")
        .toUpperCase()}_SUBGRAPH_ENDPOINT`;
    if (process.env[overrideEndpointVarName]) {
        return process.env[overrideEndpointVarName];
    }

    // else the template endpoint must be set
    if (process.env.SUBGRAPH_ENDPOINT_TEMPLATE) {
        return process.env.SUBGRAPH_ENDPOINT_TEMPLATE.replace(
            "{{NETWORK}}",
            networkName
        );
    }
    return undefined;
}

const getConfigData = (chainId) => {
    const networkData = sfMetadata.getNetworkByChainId(chainId);
    if (!networkData) {
        console.log(`no metadata found for network with chainId ${chainId}`);
        return {
            isTestnet: true,
            nativeTokenSymbol: "ETH",
        };
    }

    return {
        isTestnet: networkData.isTestnet,
        nativeTokenSymbol: networkData.nativeTokenSymbol,
        resolverAddress: networkData.contractsV1.resolver,
        subgraphQueryEndpoint: getSubgraphEndpoint(networkData.name),
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
