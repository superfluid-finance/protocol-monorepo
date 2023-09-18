const sfMetadata = require("@superfluid-finance/metadata");

module.exports = function getConfig(chainId) {

    const EXTRA_CONFIG = {
        // here go the trusted forwarders which aren't part of the framework contracts

        // Local Testing
        4447: {
            // for local testing (truffle internal ganache and TestEnvironment)
            // this is a fake forwarder address, it is to test the deployment script
            trustedForwarders: ["0x3075b4dc7085C48A14A5A39BBa68F58B19545971"],
        },
        5777: {
            // for local testing (external ganache)
            // this is a fake forwarder address, it is to test the deployment script
            trustedForwarders: ["0x3075b4dc7085C48A14A5A39BBa68F58B19545971"],
        },
        6777: {
            // for coverage testing
            // this is a fake forwarder address, it is to test the deployment script
            trustedForwarders: ["0x3075b4dc7085C48A14A5A39BBa68F58B19545971"],
        },

        // Celo Mainnet
        42220: {
            gov_enableAppWhiteListing: false,
        }
    };

    const sfNw = sfMetadata.getNetworkByChainId(chainId);
    // if no entry exists in metadata, it's probably a devnet and gets the same default params as testnets
    return {
        isTestnet: process.env.IS_TESTNET || !sfNw || sfNw.isTestnet ? true : false,
        // default liquidation period is 4h for mainnets, 1h for testnets
        liquidationPeriod: 3600 * (!sfNw || sfNw.isTestnet ? 1 : 4),
        // default patrician period is 20% of theliquidation period
        patricianPeriod: 3600 * (!sfNw || sfNw.isTestnet ? 1 : 4) * 0.2,
        gov_enableAppWhiteListing: !sfNw || sfNw.isTestnet ? false : true,
        // mainnets don't use the TestGovernance contract
        disableTestGovernance: process.env.DISABLE_TEST_GOVERNANCE || !sfNw || sfNw.isTestnet ? false : true,
        // default token list for the test deployments (empty for mainnets)
        tokenList: !sfNw || sfNw.isTestnet ? ["fDAIx", "fUSDCx", "fTUSDx"] : [],
        data: {
            initialBlockNumber: sfNw?.startBlockV1 || 0,
            getLogsRange: sfNw?.logsQueryRange || 5000,
        },
        cfaFwd: sfNw?.contractsV1?.cfaV1Forwarder || "0xcfA132E353cB4E398080B9700609bb008eceB125",
        gdaFwd: sfNw?.contractsV1?.gdaV1Forwarder || "0x6dA170169d5Fca20F902b7E5755346a97c94B07c",
        nativeTokenSymbol: sfNw?.nativeTokenSymbol || "ETH",
        metadata: sfNw,
        resolverAddress: global?.process.env.RESOLVER_ADDRESS || sfNw?.contractsV1?.resolver,
        trustedForwarders: sfNw?.trustedForwarders,
        ...EXTRA_CONFIG[chainId]
    };
};
