const DEFAULT_CONFIG = {
    5: { // goerli
        resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E"
    },
    4 : { // rinkeby
        resolverAddress: "0x659635Fab0A0cef1293f7eb3c7934542B6A6B31A"
    },
    3 : { // ropsten
        resolverAddress: "0x3b44e06D96BcA9412CBc23F80F41B9e30933571a",
    },
    80001: { // (matic) mumbai
        resolverAddress: "0x8C54C83FbDe3C59e59dd6E324531FB93d4F504d3"
    }
};

/// @dev Get the network configuration
module.exports = function getConfig(chainId) {
    const defaultConfig = DEFAULT_CONFIG[chainId] || {};
    return  {
        resolverAddress: process.env.TEST_RESOLVER_ADDRESS || defaultConfig.resolverAddress
    };
};
