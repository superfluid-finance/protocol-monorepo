//const { web3tx } = require("@decentral.ee/web3-helpers");
/**
 * @dev Deploy the superfluid framework and test tokens for local testing
 *
 * Usage: npx truffle exec scripts/deploy-all.js
 */
module.exports = async function (callback) {

    const errorHandler = err => { if (err) throw err; };
    //const deployTestToken = require("./deploy-test-token");
    //const deploySuperToken = require("./deploy-super-token");

    try {
        global.web3 = web3;
        console.log("First deployment");
        const deploy = require("./deploy");
        await deploy(errorHandler);
        callback();
    } catch (err) {
        callback(err);
    }
};
