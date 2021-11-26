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
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/create-new-app-registration-key.js : {DEPLOYER} {REGISTRATION_KEY}
 */
module.exports = eval(`(${S.toString()})({
    doNotPrintColonArgs: true
})`)(async function (args, options = {}) {
    console.log("======== Creating new app registration key ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const registrationkey = args.pop();
    const deployer = args.pop();
    console.log("Deployer", deployer);

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "SuperfluidGovernanceBase",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const appKey = web3.utils.sha3(
        web3.eth.abi.encodeParameters(
            ["string", "address", "string"],
            [
                "org.superfluid-finance.superfluid.appWhiteListing.registrationKey",
                deployer,
                registrationkey,
            ]
        )
    );
    console.log("App key", appKey);

    await sendGovernanceAction(sf, (gov) =>
        gov.whiteListNewApp(sf.host.address, appKey)
    );
});
