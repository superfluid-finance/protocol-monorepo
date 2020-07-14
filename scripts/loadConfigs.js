module.exports = () => ({
    1: { // mainnet
    },
    42: { // kovan
        resolverAddress: "0x6258d03724c90138baf05Ed7bb438a037C7bA6E4"
    },
    5: { // goerli
        resolverAddress: "0xDC200aA39Aa1D9B28CE458979602eb79046A1C9f",
    },
    4447: {
        resolverAddress: process.env.TEST_RESOLVER_ADDRESS,
    },
    5777: { // ganache 
        resolverAddress: process.env.GANACHE_RESOLVER_ADDRESS,
    }
});
