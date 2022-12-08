const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Set the 3Ps config for a super token.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/gov-set-3Ps-config.js : {TOKEN ADDRESS} {LIQUIDATION PERIOD} {PATRICIAN PERIOD}
 *        use TOKEN ADDRESS 0x0000000000000000000000000000000000000000 to set a default/fallback value
 *        if LIQUIDATION PERIOD and PATRICIAN PERIOD are zero, the current config for the token is cleared
 *        and thus falls back to the default.
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Setting 3Ps config ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 3) {
        throw new Error("Wrong number of arguments");
    }
    const patricianPeriod = parseInt(args.pop());
    const liquidationPeriod = parseInt(args.pop());
    const tokenAddr = args.pop();
    console.log("token address", tokenAddr);
    console.log("liquidation period", liquidationPeriod);
    console.log("patrician period", patricianPeriod);

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

    if (liquidationPeriod !== 0 || patricianPeriod !== 0) {
        console.log("setting new 3Ps config");
        await sendGovernanceAction(sf, (gov) =>
            gov.setPPPConfig(
                sf.host.address,
                tokenAddr,
                liquidationPeriod,
                patricianPeriod
            )
        );
    } else {
        console.log("clearing 3Ps config");
        await sendGovernanceAction(sf, (gov) =>
            gov.clearPPPConfig(sf.host.address, tokenAddr)
        );
    }
});
