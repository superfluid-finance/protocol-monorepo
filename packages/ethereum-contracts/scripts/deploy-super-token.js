const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    ZERO_ADDRESS,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader
} = require("./utils");

/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : {TOKEN_NAME}
 */
module.exports = async function(callback, argv, options = {}) {
    try {
        console.log("Deploying super token");

        eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        const version = process.env.RELEASE_VERSION || "test";
        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();
        console.log("Underlying token name", tokenName);

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version,
            additionalContracts: ["TestResolver", "UUPSProxiable", "SETHProxy"],
            contractLoader: builtTruffleContractLoader
        });
        await sf.initialize();

        const {
            TestResolver,
            UUPSProxiable,
            ISuperfluidGovernance,
            ISuperToken,
            ISETH,
            SETHProxy
        } = sf.contracts;

        const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
            await sf.host.getSuperTokenFactory.call()
        );

        let deploymentFn;
        if (tokenName == "ETH") {
            deploymentFn = async () => {
                console.log("Creating SETH Proxy...");
                const sethProxy = await SETHProxy.new(ZERO_ADDRESS);
                const seth = await ISETH.at(sethProxy.address);
                console.log("Intialize SETH as a custom super token...");
                await superTokenFactory.initializeCustomSuperToken(
                    seth.address
                );
                console.log("Intialize SETH token info...");
                await seth.initialize(ZERO_ADDRESS, 18, "Super ETH", "ETHx");
                return seth;
            };
        } else {
            const tokenAddress = await sf.resolver.get(`tokens.${tokenName}`);
            const tokenInfo = await sf.contracts.TokenInfo.at(tokenAddress);
            const tokenInfoName = await tokenInfo.name.call();
            const tokenInfoSymbol = await tokenInfo.symbol.call();
            const tokenInfoDecimals = await tokenInfo.decimals.call();
            console.log("Token address", tokenAddress);
            console.log("Token name", tokenName);
            console.log("Token info name()", tokenInfoName);
            console.log("Token info symbol()", tokenInfoSymbol);
            console.log("Token info decimals()", tokenInfoDecimals.toString());
            deploymentFn = async () => {
                return await sf.createERC20Wrapper(tokenInfo);
            };
        }

        const name = `supertokens.${version}.${tokenName}x`;
        const superTokenAddress = await sf.resolver.get(name);
        console.log("SuperToken namt at the resolver: ", name);
        console.log("SuperToken address: ", superTokenAddress);
        let doDeploy = false;
        if (superTokenAddress == ZERO_ADDRESS) {
            doDeploy = true;
        } else {
            console.log("The superToken already registered.");
            const superToken = await ISuperToken.at(superTokenAddress);
            if ((await superToken.getHost()) !== sf.host.address) {
                console.log(
                    "But the superToken uses a different host, redeploying is required."
                );
                doDeploy = true;
            } else {
                const superTokenLogic1 = await superTokenFactory.getSuperTokenLogic();
                console.log(
                    "Latest SuperToken logic address",
                    superTokenLogic1
                );
                const superTokenLogic2 = await (
                    await UUPSProxiable.at(superTokenAddress)
                ).getCodeAddress();
                console.log(
                    "Current SuperToken logic address",
                    superTokenLogic2
                );
                if (superTokenLogic1 !== superTokenLogic2) {
                    console.log("SuperToken logic needs to be updated.");
                    console.log("Updating supertoken's logic....");
                    const gov = await ISuperfluidGovernance.at(
                        await sf.host.getGovernance()
                    );
                    await gov.updateSuperTokenLogic(
                        sf.host.address,
                        superTokenAddress
                    );
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
            }
        }
        if (doDeploy) {
            console.log("Creating the wrapper...");
            const superToken = await deploymentFn();
            console.log("Wrapper created at", superToken.address);
            console.log("Resolver setting new address...");
            const testResolver = await TestResolver.at(sf.resolver.address);
            await testResolver.set(name, superToken.address);
            console.log("Resolver set done.");
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
