const DEFAULT_CONFIG = {
    5: {
        // goerli
        resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E"
    },
    4: {
        // rinkeby
        resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A"
    },

    3: {
        // ropsten
        resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a"
    },
    42: {
        // kovan
        resolverAddress: "0x851d3dd9dc97c1df1DA73467449B3893fc76D85B"
    },
    80001: {
        // (matic) mumbai testnet
        resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3"
    },
    0x03c401: {
        // (artis) tau1 testnet
        resolverAddress: "0x79D426CD219eDCFEB2dCbcf7ea0F8B3642C56F47"
    }
};

/// @dev Get the network configuration
module.exports = function getConfig(chainId) {
    const defaultConfig = DEFAULT_CONFIG[chainId] || {};
    return {
        resolverAddress:
            process.env.TEST_RESOLVER_ADDRESS || defaultConfig.resolverAddress
    };
};
