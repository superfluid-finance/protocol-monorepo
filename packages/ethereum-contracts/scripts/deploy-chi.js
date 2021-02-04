const util = require("util");
const TruffleContract = require("@truffle/contract");
const ChiToken = TruffleContract(require("./ChiToken.json"));
const { hasCode } = require("./utils");

const DEPLOYER_ACCOUNT = "0x7E1E3334130355799F833ffec2D731BCa3E68aF6";
const CHI_TOKEN_ADDRESS = "0x0000000000004946c0e9F43F4Dee607b0eF1fA1c";

/*
 * @dev Deploy CHI Token to the (ganache) network.
 * @param web3 The web3 to be used
 * @param from address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-chi.js
 */
module.exports = async function(callback, { web3, from } = {}) {
    try {
        if (!from) {
            const accounts = await this.web3.eth.getAccounts();
            from = accounts[0];
        }

        if (!(await hasCode(web3, CHI_TOKEN_ADDRESS))) {
            console.log("Deploying Chi token...");
            ChiToken.setProvider(web3.currentProvider);
            await web3.eth.sendTransaction({
                from,
                to: DEPLOYER_ACCOUNT,
                value: "1" + "0".repeat(16)
            });
            // use ganache specific method to unlock unkonwn account for
            // testing purpose
            await util.promisify(web3.currentProvider.send)({
                jsonrpc: "2.0",
                method: "evm_unlockUnknownAccount",
                params: [DEPLOYER_ACCOUNT]
            });
            const chi = await ChiToken.new({
                from: DEPLOYER_ACCOUNT
            });
            console.log(`Chi token deployed to ${chi.address}`);
        } else {
            console.log(`Chi token already deployed to ${CHI_TOKEN_ADDRESS}`);
        }
        callback();
    } catch (err) {
        callback(err);
    }
};
