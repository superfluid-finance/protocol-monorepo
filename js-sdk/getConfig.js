const DEFAULT_CONFIG = {
    5: {
        resolverAddress: "0x3710AB3fDE2B61736B8BB0CE845D6c61F667a78E"
    }
};

/// @dev Get the network configuration
module.exports = function getConfig(chainId) {
    const defaultConfig = DEFAULT_CONFIG[chainId] || {};
    return  {
        resolverAddress: process.env.TEST_RESOLVER_ADDRESS || defaultConfig.resolverAddress
    };
};
