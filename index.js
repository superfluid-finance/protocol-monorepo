const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("./build/abi");

module.exports = {
    load: (provider) => {
        let contracts = {};
        Object.keys(SuperfluidABI).forEach(i => {
            contracts[i] = TruffleContract({
                contractName: i,
                abi: SuperfluidABI[i]
            });
        });
        Object.values(contracts).forEach(i => i.setProvider(provider));
        return contracts;
    },
    getConfig: (netId) => {
        return ({
            5: { // goerli
                resolverAddress: "0xDC200aA39Aa1D9B28CE458979602eb79046A1C9f"
            },
            42: { // kovan
                resolverAddress: "0x6258d03724c90138baf05Ed7bb438a037C7bA6E4"
            },
        })[netId]
        || ({ // test environment
            resolverAddress: process.env.TEST_RESOLVER_ADDRESS
        });
    }
};
