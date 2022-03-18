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
            // for local testing (truffle internal ganache and TestEnvironment)
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
            patricianPeriod: 3600 * 0.2,
            biconomyForwarder: "0x3075b4dc7085C48A14A5A39BBa68F58B19545971",
        },
        4: {
            // rinkeby
            liquidationPeriod: 3600,
            patricianPeriod: 3600 * 0.2,
            biconomyForwarder: "0x1730cAe53340aB01228019618C2b544642f3650A",
        },

        3: {
            // ropsten
            liquidationPeriod: 3600,
            patricianPeriod: 3600 * 0.2,
            biconomyForwarder: "0x1De6349B96774ed5E3569e47D609C19A8dE15C89",
        },
        42: {
            // kovan
            liquidationPeriod: 3600,
            patricianPeriod: 3600 * 0.2,
            biconomyForwarder: "0xE8Df44bcaedD41586cE73eB85e409bcaa834497B",
        },

        //
        // MATIC: https://docs.matic.network/docs/develop/network-details/network/
        //
        80001: {
            // (matic) mumbai testnet
            liquidationPeriod: 3600,
            patricianPeriod: 3600 * 0.2,
            biconomyForwarder: "0x2B99251eC9650e507936fa9530D11dE4d6C9C05c",
        },
        137: {
            // (matic) mainnet
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["DAIx", "USDCx", "ETHx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
            data: {
                initialBlockNumber: 10000000,
                getLogsRange: 10000,
            },
        },

        //
        // xDAI: https://www.xdaichain.com/for-users/wallets/metamask/metamask-setup
        //
        100: {
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["ETHx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
        },

        //
        // BSC: https://docs.binance.org/smart-chain/developer/rpc.html
        //
        56: {
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: [],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
            data: {
                initialBlockNumber: 5000000,
                getLogsRange: 5000,
            },
        },

        //
        // Optimistic Ethereum: https://community.optimism.io/docs/
        //
        10: {
            // optimism mainnet
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["DAIx", "USDCx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
            data: {
                initialBlockNumber: 4300000,
                getLogsRange: 50000,
            },
        },
        69: {
            // optimism testnet (kovan)
            data: {
                initialBlockNumber: 300000,
                getLogsRange: 50000,
            },
        },

        //
        // Arbitrum: https://developer.offchainlabs.com
        //
        42161: {
            // arbitrum one mainnet
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["DAIx", "USDCx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
            data: {
                initialBlockNumber: 7600000,
                getLogsRange: 50000,
            },
        },
        421611: {
            // arbitrum testnet (rinkeby)
            data: {
                initialBlockNumber: 7300000,
                getLogsRange: 50000,
            },
        },

        //
        // ARTIS
        //
        0x03c401: {
            // (artis) tau1 testnet
        },

        //
        // Avalanche C-Chain: https://docs.avax.network/learn/platform-overview#contract-chain-c-chain
        //
        43114: {
            // avalanche mainnet
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["AVAXx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
            data: {
                initialBlockNumber: 11950000,
                getLogsRange: 50000,
            },
        },
        43113: {
            // avalanche fuji testnet
            data: {
                initialBlockNumber: 3220000,
                getLogsRange: 50000,
            },
        },

        //
        // Celo: https://github.com/celo-org/celo-monorepo#docs
        //
        42220: {
            // celo mainnet
            liquidationPeriod: 3600 * 4,
            patricianPeriod: 3600 * 4 * 0.2,
            tokenList: ["CELOx"],
            // governance default configs
            gov_enableAppWhiteListing: true,
            // misc
            disableTestGovernance: true,
        },
    };

    return {
        // global default configs
        ...{
            // default liquidation period for the test deployments
            liquidationPeriod: 3600,
            // default patrician period for test deployments
            patricianPeriod: 3600 * 0.2,
            // default token list for the test deployments
            tokenList: ["fDAIx", "fUSDCx", "fTUSDx"],
            data: {},
        },
        // network specific configs
        ...DEFAULT_CONFIGS[chainId],
        // SDK provided configs
        ...SuperfluidSDK.getConfig(chainId),
    };
};
