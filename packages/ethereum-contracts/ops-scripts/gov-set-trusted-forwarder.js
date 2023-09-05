const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Enable or disable a trusted forwarder
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3 Injected web3 instance
 * @param {Address} options.from Address to send transactions from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec ops-scripts/gov-set-trusted-forwarder.js : {TOKEN ADDRESS} {FORWARDER ADDRESS} {ENABLE}
 *        use TOKEN ADDRESS 0x0000000000000000000000000000000000000000 to set for all tokens.
 *        If ENABLE is 1, the forwarder is enabled; if ENABLE is 0, the forwarder is disabled.
 *
 * ENV vars:
 *     GOVERNANCE_ADMIN_TYPE needs to be set to MULTISIG for networks with multisig owned governance
 *
 * Make sure to only set forwarders which can be fully trusted!
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Setting trusted forwarder ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 3) {
        throw new Error("Wrong number of arguments");
    }
    const enable = parseInt(args.pop());
    const forwarderAddr = args.pop();
    const tokenAddr = args.pop();
    console.log("token address", tokenAddr);
    console.log("forwarder address", forwarderAddr);
    console.log("enable", enable);

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

    if (enable === 1) {
        console.log(
            `Enabling trusted forwarder ${forwarderAddr} for ${tokenAddr}`
        );
        await sendGovernanceAction(sf, (gov) =>
            gov.enableTrustedForwarder(
                sf.host.address,
                tokenAddr,
                forwarderAddr
            )
        );
    } else if (enable === 0) {
        console.log(
            `Disabling trusted forwarder ${forwarderAddr} for ${tokenAddr}`
        );
        await sendGovernanceAction(sf, (gov) =>
            gov.disableTrustedForwarder(
                sf.host.address,
                tokenAddr,
                forwarderAddr
            )
        );
    } else {
        console.error("invalid value for argument ENABLE provided!");
    }
});
