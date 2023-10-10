const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Authorizes an account to get Super Apps registered.
 * For more details, see https://github.com/superfluid-finance/protocol-monorepo/wiki/Super-App-White-listing-Guide
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage:
 * npx truffle exec ops-scripts/gov-authorize-app-deployer.js : {DEPLOYER} [EXPIRATION_TS]
 *        EXPIRATION_TS is a Unix timestamp in seconds for when the authorization should expire.
 *        If not set, we default to (for practical purposed) no expiration.
 *        Hint: you may use https://www.unixtimestamp.com/ to calculate a timestamp
 */
module.exports = eval(`(${S.toString()})({
    doNotPrintColonArgs: true
})`)(async function (args, options = {}) {
    console.log("======== Authorizing Super App Deployer ========");
    let {protocolReleaseVersion} = options;

    if (args.length > 3 || args.length < 2) {
        throw new Error("Wrong number of arguments");
    }

    // default: 2^64 - 1 (far in the future - for practical purposes, never expiring)
    let expirationTs = (BigInt(2) ** BigInt(64) - BigInt(1)).toString();
    if (args.length === 3) {
        const expTsStr = args.pop();
        const parsedExpTs = parseInt(expTsStr);
        if (parsedExpTs.toString() !== expTsStr) {
            throw new Error("EXPIRATON_TS not an integer");
        }
        expirationTs = parsedExpTs;
        console.log("Expiration timestamp", expirationTs);
        console.log("Expiration date", new Date(expirationTs * 1000)); // print human readable
    }
    // for historical reasons, we have "registration keys" and now hardcode those to "k1"
    const registrationKey = "k1";
    const deployer = args.pop();
    console.log("Deployer", deployer);

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

    await sendGovernanceAction(sf, (gov) =>
        gov.setAppRegistrationKey(
            sf.host.address,
            deployer,
            registrationKey,
            expirationTs
        )
    );
});
