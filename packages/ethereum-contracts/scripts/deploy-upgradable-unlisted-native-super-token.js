const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
} = require("./libs/common");

/**
 * @dev Deploy unlisted native super token to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-upgradable-unlisted-native-super-token.js : {NAME} {SYMBOL} {INITIAL SUPPLY}
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log(
        "======== Deploying upgradable unlisted native super token ========"
    );
    let { protocolReleaseVersion } = options;

    if (args.length !== 3) {
        throw new Error("Wrong number of arguments");
    }
    const initialSupply = args.pop();
    const superTokenSymbol = args.pop();
    const superTokenName = args.pop();
    console.log("Super token name", superTokenName);
    console.log("Super token symbol", superTokenSymbol);
    console.log("Super token initial supply", initialSupply);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "AuxNativeSuperTokenLogicV1",
            "UpgradableNativeSuperTokenProxy",
            "INativeSuperToken",
        ],
        contractLoader: builtTruffleContractLoader,
    });
    await sf.initialize();

    const {
        AuxNativeSuperTokenLogicV1,
        UpgradableNativeSuperTokenProxy,
        INativeSuperToken,
    } = sf.contracts;

    const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
        await sf.host.getSuperTokenFactory.call()
    );

    console.log("Deploying AuxNativeSuperTokenLogic...");
    const auxLogic = await AuxNativeSuperTokenLogicV1.new();

    console.log("Deploying UpgradableNativeSuperTokenProxy...");
    const proxy = await UpgradableNativeSuperTokenProxy.new(auxLogic.address);

    const tokenAux = await AuxNativeSuperTokenLogicV1.at(proxy.address);
    console.log("Initializing AuxNativeSuperTokenLogic...");
    await tokenAux.initializeAux(42);
    // this would fail (already initialized)
    //await tokenAux.initializeAux(52);

    const token = await INativeSuperToken.at(proxy.address);

    console.log("Invoking initializeCustomSuperToken...");
    await superTokenFactory.initializeCustomSuperToken(token.address);

    console.log("Invoking initialize...");
    await token.initialize(
        superTokenName,
        superTokenSymbol,
        web3.utils.toWei(String(initialSupply))
    );

    console.log(`Upgradable Native SuperToken deployed at ${token.address}`);

    // calls routed to auxLogic
    await tokenAux.randomFun();
    const theAnswer = await tokenAux.getSecret();
    console.log(`the answer to the question: ${theAnswer}`);

    // call routed to SuperToken implementation
    const totalSupply = await token.totalSupply();
    console.log(`totalSupply: ${totalSupply}`);
});
