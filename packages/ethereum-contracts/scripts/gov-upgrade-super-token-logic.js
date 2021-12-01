const async = require("async");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const MAX_REQUESTS = 100;

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Upgrade a managed super token to the latest logic
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/gov-upgrade-super-token-logic.js : ALL | {SUPER_TOKEN_ADDRESS} ...
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Upgrade super token logic ========");
    let { protocolReleaseVersion } = options;

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "SuperfluidGovernanceBase",
            "TestResolver",
            "UUPSProxiable",
            "SETHProxy",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const { UUPSProxiable, ISuperToken } = sf.contracts;

    const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
        await sf.host.getSuperTokenFactory.call()
    );

    let tokensToBeChecked;
    if (args.length === 1 && args[0] === "ALL") {
        tokensToBeChecked = (
            await sf.subgraphQuery(`{
            tokenStatistics(first: 1000) {
                token {
                    id
                }
            }
        }`)
        ).tokenStatistics.map((i) => i.token.id);
    } else {
        tokensToBeChecked = Array.from(args);
    }

    const latestSuperTokenLogic = await superTokenFactory.getSuperTokenLogic();
    console.log("Latest SuperToken logic address", latestSuperTokenLogic);

    let tokensToBeUpgraded = (
        await async.mapLimit(
            tokensToBeChecked,
            MAX_REQUESTS,
            async (superTokenAddress) => {
                const superToken = await ISuperToken.at(superTokenAddress);
                const symbol = await superToken.symbol();
                if ((await superToken.getHost()) !== sf.host.address) {
                    throw new Error("Super token is from a different universe");
                }
                const superTokenLogic = await (
                    await UUPSProxiable.at(superTokenAddress)
                ).getCodeAddress();

                if (latestSuperTokenLogic !== superTokenLogic) {
                    console.log(
                        `SuperToken@${superToken.address} (${symbol}) logic needs to be updated from ${superTokenLogic}`
                    );
                    return superTokenAddress;
                } else {
                    console.log(
                        `SuperToken@${superToken.address} (${symbol}) logic is up to date`
                    );
                    return undefined;
                }
            }
        )
    ).filter((i) => typeof i !== "undefined");

    if (tokensToBeUpgraded.length > 0) {
        await sendGovernanceAction(sf, (gov) =>
            gov.batchUpdateSuperTokenLogic(sf.host.address, tokensToBeUpgraded)
        );
    }
});
