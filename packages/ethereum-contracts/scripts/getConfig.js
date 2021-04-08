const SuperfluidSDK = require("@superfluid-finance/js-sdk");

/*
 * REFERENCES:
 * - https://docs.biconomy.io/misc/contract-addresses
 */

module.exports = function getConfig(chainId) {
    const DEFAULT_CONFIGS = {
        //
        // Local Testing
        //
        4447: {
            // for local testing (truffle internal ganache)
            // this is a fake forwarder address, it is to test the deployment script
            biconomyForwarder: "0x3075b4dc7085C48A14A5A39BBa68F58B19545971",
        },
        5777: {
            // for local testing (external ganache)
            // this is a fake forwarder address, it is to test the deployment script
            biconomyForwarder: "0x3075b4dc7085C48A14A5A39BBa68F58B19545971",
        },
        6777: {
            // for coverage testing
            // this is a fake forwarder address, it is to test the deployment script
            biconomyForwarder: "0x3075b4dc7085C48A14A5A39BBa68F58B19545971",
        },

        //
        // ETHEREUM
        //
        5: {
            // goerli
            liquidationPeriod: 3600,
            biconomyForwarder: "0x3075b4dc7085C48A14A5A39BBa68F58B19545971",
        },
        4: {
            // rinkeby
            liquidationPeriod: 3600,
            biconomyForwarder: "0x1730cAe53340aB01228019618C2b544642f3650A",
        },

        3: {
            // ropsten
            liquidationPeriod: 3600,
            biconomyForwarder: "0x1De6349B96774ed5E3569e47D609C19A8dE15C89",
        },
        42: {
            // kovan
            liquidationPeriod: 3600,
            biconomyForwarder: "0xE8Df44bcaedD41586cE73eB85e409bcaa834497B",
        },

        //
        // MATIC: https://docs.matic.network/docs/develop/network-details/network/
        //
        137: {
            // (matic) mainnet
            liquidationPeriod: 3600 * 4,
            testTokens: [],
        },
        80001: {
            // (matic) mumbai testnet
            liquidationPeriod: 3600,
            biconomyForwarder: "0x2B99251eC9650e507936fa9530D11dE4d6C9C05c",
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        0x64: {
            liquidationPeriod: 3600 * 4,
            testTokens: [],
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //

        //
        // ARBITRUM
        //
        79377087078960: {
            // arbitrum testnet
        },

        //
        // ARTIS
        //
        0x03c401: {
            // (artis) tau1 testnet
        },
    };

    return {
        // global default configs
        ...{
            liquidationPeriod: 3600,
            testTokens: ["fDAI", "fUSDC", "fTUSD"],
        },
        // network specific configs
        ...DEFAULT_CONFIGS[chainId],
        // SDK provided configs
        ...SuperfluidSDK.getConfig(chainId),
    };
};
