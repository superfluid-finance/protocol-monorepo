/// @dev Get the network configuration
module.exports = function getConfig(chainId) {
    return ({
        5: { // goerli
            resolverAddress: process.env.TEST_RESOLVER_ADDRESS || "0xDC200aA39Aa1D9B28CE458979602eb79046A1C9f"
        },
        42: { // kovan
            resolverAddress: process.env.TEST_RESOLVER_ADDRESS || "0x6258d03724c90138baf05Ed7bb438a037C7bA6E4"
        },
    })[chainId]
    || ({ // test environment
        resolverAddress: process.env.TEST_RESOLVER_ADDRESS
    });
};
