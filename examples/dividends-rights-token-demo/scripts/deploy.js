const { web3tx, setWeb3Provider } = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
//const deployTestEnvironment = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-environment");
//const deployTestEnvironment = require("../../../packages/ethereum-contracts/scripts/deploy-test-environment");
const DividendRightsToken = artifacts.require("DividendRightsToken");

module.exports = async function(callback) {
    const errorHandler = err => {
        if (err) throw err;
    };

    try {
        const version = process.env.RELEASE_VERSION || "test";
        console.log("release version:", version);

        // FIXME to be enabled
        if (process.env.DEPLOY_SUPERFLUID_TEST_ENVIRONMENT) {
            const accounts = await web3.eth.getAccounts();
            await deployTestEnvironment(errorHandler, {
                web3,
                version,
                from: accounts[0]
            });
        }

        // make sure that we are using the same web3 provider in the helpers
        setWeb3Provider(web3.currentProvider);

        const sf = new SuperfluidSDK.Framework({
            web3,
            version,
            tokens: ["fDAI"]
        });
        await sf.initialize();

        const app = await web3tx(
            DividendRightsToken.new,
            "Deploy DividendRightsToken"
        )(
            "Dividend Rights Token",
            "DRT",
            sf.tokens.fDAIx.address,
            sf.host.address,
            sf.agreements.ida.address
        );
        console.log("App deployed at", app.address);
        callback();
    } catch (err) {
        callback(err);
    }
};
