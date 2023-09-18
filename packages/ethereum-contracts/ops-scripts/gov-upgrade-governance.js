const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./libs/common");

/**
 * @dev Deploy the latest governance logic and initiate an upgrade to it.
 * Needs the deployed governance contract to implement UUPSProxiable
 * and owned by a Gnosis Multisig Wallet.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {string} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec ops-scripts/gov-upgrade-governance.js
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Upgrade governance ========");
    let {protocolReleaseVersion} = options;

    console.log("protocol release version:", protocolReleaseVersion);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "Ownable",
            "IMultiSigWallet",
            "ISafe",
            "SuperfluidGovernanceBase",
            "SuperfluidGovernanceII",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const gov = await sf.contracts.UUPSProxiable.at(
        await sf.host.getGovernance.call()
    );
    console.log("Governance address:", gov.address);

    // sanity check - do we have an upgradable proxy contract and the right one?
    try {
        const uuidIs = await gov.proxiableUUID();
        const uuidWant = web3.utils.keccak256(
            "org.superfluid-finance.contracts.SuperfluidGovernanceII.implementation"
        );
        if (uuidIs !== uuidWant) {
            console.error("ERR: governance contract: proxiableUUID mismatch");
            process.exit(1);
        }
    } catch (e) {
        console.error(
            "ERR: governance seems not to be an instance of UUPSProxiable"
        );
        process.exit(1);
    }

    // we have the right contract, proceeding with new contract deployment
    console.log(
        "Deploying new instance of SuperfluidGovernanceII (gov logic)..."
    );
    const govLogic = await sf.contracts.SuperfluidGovernanceII.new();
    console.log("Deployed to", govLogic.address);
    await govLogic.castrate();
    console.log("Marked gov logic as initialized (castrate)");

    await sendGovernanceAction(sf, (govMethods) =>
        govMethods.updateCode(govLogic.address)
    );
});
