const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./utils");

/**
 * @dev Upgrade a managed super token to the latest logic
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/upgrade-super-token-logic.js : ${SUPER_TOKEN_ADDRESS}
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Upgrade super token logic ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const superTokenAddress = args.pop();

        protocolReleaseVersion =
            protocolReleaseVersion || process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("chain ID: ", chainId);
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
        const superToken = await ISuperToken.at(superTokenAddress);
        if ((await superToken.getHost()) !== sf.host.address) {
            throw new Error("Super token is from a different universe");
        }

        const superTokenLogic1 = await superTokenFactory.getSuperTokenLogic();
        console.log("Latest SuperToken logic address", superTokenLogic1);
        const superTokenLogic2 = await (
            await UUPSProxiable.at(superTokenAddress)
        ).getCodeAddress();
        console.log("Current SuperToken logic address", superTokenLogic2);

        if (superTokenLogic1 !== superTokenLogic2) {
            console.log("SuperToken logic needs to be updated.");
            await sendGovernanceAction(sf, (gov) =>
                gov.updateSuperTokenLogic(sf.host.address, superTokenAddress)
            );
            if (!process.env.GOVERNANCE_TYPE) {
                // validate the token logic update for default governance type updates
                const superTokenLogic3 = await (
                    await UUPSProxiable.at(superTokenAddress)
                ).getCodeAddress();
                console.log(
                    "Updated SuperToken logic address",
                    superTokenLogic3
                );
                if (superTokenLogic3 !== superTokenLogic1)
                    throw new Error("SuperToken logic not updated");
                console.log("SuperToken's logic has been updated.");
            }
        } else {
            console.log("SuperToken's logic is up to date.");
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
