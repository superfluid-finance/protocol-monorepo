const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Create a new super app factory registration.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/create-new-factory-registration.js : {FACTORY_ADDRESS}
 */
module.exports = eval(`(${S.toString()})({
    doNotPrintColonArgs: true
})`)(async function (args, options = {}) {
    console.log("======== Creating new factory registration ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 1) {
        throw new Error("Wrong number of arguments");
    }
    const factoryAddress = args.pop();

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

    console.log("Factory Address:", factoryAddress);

    await sendGovernanceAction(sf, (gov) =>
        gov.authorizeAppFactory(sf.host.address, factoryAddress)
    );
});
