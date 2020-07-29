const TruffleContract = require("@truffle/contract");
const SuperfluidABI = require("./build/abi");

module.exports = {
    /// @dev Load contracts (TruffleContract) from ABIs and bind to the provider
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
    /// @dev Get the ERC20 wrapper for the token
    getERC20Wrapper: async (registry, tokenInfo) => {
        const tokenInfoSymbol = await tokenInfo.symbol.call();
        const tokenInfoDecimals = await tokenInfo.decimals.call();
        return await registry.getERC20Wrapper.call(
            `${tokenInfoSymbol}x`,
            tokenInfoDecimals.toString(),
            tokenInfo.address
        );
    },
    /// @dev Get the network configuration
    getConfig: (chainId) => {
        return ({
            5: { // goerli
                resolverAddress: "0xDC200aA39Aa1D9B28CE458979602eb79046A1C9f"
            },
            42: { // kovan
                resolverAddress: "0x6258d03724c90138baf05Ed7bb438a037C7bA6E4"
            },
        })[chainId]
        || ({ // test environment
            resolverAddress: process.env.TEST_RESOLVER_ADDRESS
        });
    }
};
