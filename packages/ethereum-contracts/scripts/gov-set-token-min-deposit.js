const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Set the minimum deposit for a super token.
 * @param {Array} args Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3 Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/gov-set-token-min-deposit.js : {TOKEN ADDRESS} {MINIMUM DEPOSIT}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Setting minimum deposit ========");
    let {protocolReleaseVersion} = options;

    if (args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }
    const tokenAddr = args.pop();
    const minimumDeposit = args.pop();
    console.log("token address", tokenAddr);
    console.log("minimum deposit", minimumDeposit);

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

    await sendGovernanceAction(sf, (gov) =>
        gov.setSuperTokenMinimumDeposit(
            sf.host.address,
            tokenAddr,
            minimumDeposit
        )
    );
});
