const SuperfluidSDK = require("@superfluid-finance/js-sdk");

/**
 * @dev Show framework info
 *
 * Usage: npx truffle exec scripts/show-framework-info.js
 */

module.exports = async function(callback) {
    global.web3 = web3;

    try {
        const tokens = ["fDAI", "fUSDC", "fTUSD"];
        const sf = new SuperfluidSDK.Framework({
            version: process.env.RELEASE_VERSION || "test",
            web3Provider: web3.currentProvider,
            tokens
        });
        await sf.initialize();
        callback();
    } catch (err) {
        callback(err);
    }
};
