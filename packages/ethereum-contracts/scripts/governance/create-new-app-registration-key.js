const { parseColonArgs, detectTruffleAndConfigure } = require("../utils");

module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Creating new app registration key ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { walletType } = options;
        walletType = walletType || "gnosis-wallet";

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 2) {
            throw new Error("Not enough arguments");
        }
        const registrationkey = args.pop();
        const deployer = args.pop();
        console.log("Wallet Type", walletType);
        console.log("Deployer", deployer);
        console.log("Registration key", registrationkey);

        const secretKey = web3.utils.sha3(
            web3.eth.abi.encodeParameters(
                ["string", "address", "string"],
                [
                    "org.superfluid-finance.superfluid.appWhiteListing.seed",
                    deployer,
                    registrationkey,
                ]
            )
        );
        console.log("Secret key", secretKey);
        callback();
    } catch (err) {
        callback(err);
    }
};
