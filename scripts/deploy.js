const { web3tx } = require("@decentral.ee/web3-test-helpers");
const configs = require("./configs");

module.exports = async function (callback) {
    try {
        global.web3 = web3;
        const network = await web3.eth.net.getNetworkType();

        console.log("network: ", network);
        const SimpleVault = artifacts.require("SimpleVault");
        const config = configs[network];

        const vault = await web3tx(SimpleVault.new, "SimpleVault.new")(
            config.token.address
        );
        console.log("vault address", vault.address);

        callback();
    } catch (err) {
        callback(err);
    }
};
