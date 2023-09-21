const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Create a new super app registration key.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Note: the key itself doesn't have much meaning, it could be "stolen" from a broadcast tx anyway.
 * But since it's bound to a deployer address, that doesn't really matter.
 *
 * Usage:
 * npx truffle exec ops-scripts/gov-create-new-app-registration-key.js : {DEPLOYER} {REGISTRATION_KEY} [EXPIRATION_TS]
 *        EXPIRATION_TS is a Unix timestamp in seconds for when the key should expire.
 *        If not set, we default to 90 days in the future.
 *        Hint: you may use https://www.unixtimestamp.com/ to calculate a timestamp
 */
module.exports = eval(`(${S.toString()})({
    doNotPrintColonArgs: true
})`)(async function (args, options = {}) {
    console.log("======== Creating new app registration key ========");
    let {protocolReleaseVersion} = options;

    if (args.length > 3 || args.length < 2) {
        throw new Error("Wrong number of arguments");
    }

    let expirationTs = Math.floor(Date.now() / 1000) + 3600 * 24 * 90; // 90 days from now
    if (args.length === 3) {
        const expTsStr = args.pop();
        const parsedExpTs = parseInt(expTsStr);
        if (parsedExpTs.toString() !== expTsStr) {
            throw new Error("EXPIRATON_TS not an integer");
        }
        expirationTs = parsedExpTs;
    }
    const registrationKey = args.pop();
    const deployer = args.pop();
    console.log("Deployer", deployer);
    console.log("Registration key", registrationKey);
    console.log("Expiration timestamp", expirationTs);

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "ISafe",
            "SuperfluidGovernanceBase",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    console.log("Expiration date", new Date(expirationTs * 1000)); // print human readable

    await sendGovernanceAction(sf, (gov) =>
        gov.setAppRegistrationKey(
            sf.host.address,
            deployer,
            registrationKey,
            expirationTs
        )
    );
});
