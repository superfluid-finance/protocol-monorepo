const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    parseColonArgs,
    ZERO_ADDRESS,
    extractWeb3Options,
    detectTruffleAndConfigure,
    builtTruffleContractLoader,
    sendGovernanceAction,
} = require("./utils");

/**
 * @dev Deploy a listed super token (ERC20 wrapper type) to the network.
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 * @param {boolean} options.protocolReleaseVersion Specify the protocol release version to be used
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : ${UNDERLYING_TOKEN_SYMBOL_OR_ADDRESS}
 *
 * NOTE:
 * - If the `UNDERLYING_TOKEN_SYMBOL_OR_ADDRESS` is the same as the nativeTokenSymbol defined in
 *   the js-sdk, then the SETH contract will be deployed.
 * - Otherwise an ERC20 super token wrapper will be created, the underlying token address
 *   has to be registered in the resolver as `tokens.${UNDERLYING_TOKEN_SYMBOL}` unless it is
 *   already a `${UNDERLYING_TOKEN_ADDRESS}`
 * - An entry in `supertokens.${protocolReleaseVersion}.${UNDERLYING_TOKEN_SYMBOL}x` will be created
 *   for the super token address.
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        console.log("======== Deploying super token ========");

        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);
        let { resetToken, protocolReleaseVersion } = options;

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenSymbolOrAddress = args.pop();

        resetToken = resetToken || !!process.env.RESET_TOKEN;
        protocolReleaseVersion =
            protocolReleaseVersion || process.env.RELEASE_VERSION || "test";
        const chainId = await web3.eth.net.getId(); // MAYBE? use eth.getChainId;
        console.log("reset token: ", resetToken);
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

        const {
            TestResolver,
            UUPSProxiable,
            ERC20WithTokenInfo,
            ISuperToken,
            ISETH,
            SETHProxy,
        } = sf.contracts;

        let tokenSymbol;
        let tokenAddress;
        if (web3.utils.isAddress(tokenSymbolOrAddress)) {
            tokenAddress = tokenSymbolOrAddress;
            tokenSymbol = await (
                await ERC20WithTokenInfo.at(tokenAddress)
            ).symbol.call();
        } else {
            tokenSymbol = tokenSymbolOrAddress;
            tokenAddress = await sf.resolver.get(`tokens.${tokenSymbol}`);
        }

        console.log("Underlying token symbol", tokenSymbol);
        console.log("Underlying token address", tokenAddress);

        const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
            await sf.host.getSuperTokenFactory.call()
        );

        let superTokenKey;
        let deploymentFn;
        if (tokenSymbol == sf.config.nativeTokenSymbol) {
            superTokenKey = `supertokens.${protocolReleaseVersion}.${tokenSymbol}x`;
            deploymentFn = async () => {
                console.log("Creating SETH Proxy...");
                const weth =
                    options.weth || process.env.WETH_ADDRESS || ZERO_ADDRESS;
                console.log("WETH address", weth);
                const sethProxy = await SETHProxy.new(weth);
                console.log("WETH Address: ", weth);
                const seth = await ISETH.at(sethProxy.address);
                console.log("Intialize SETH as a custom super token...");
                await superTokenFactory.initializeCustomSuperToken(
                    seth.address
                );
                console.log("Intialize SETH token info...");
                await seth.initialize(
                    ZERO_ADDRESS,
                    18,
                    `Super ${sf.config.nativeTokenSymbol}`,
                    `${sf.config.nativeTokenSymbol}x`
                );
                return seth;
            };
        } else {
            superTokenKey = `supertokens.${protocolReleaseVersion}.${tokenSymbol}`;
            if ((await sf.resolver.get(superTokenKey)) === ZERO_ADDRESS) {
                const tokenAddress = await sf.resolver.get(
                    `tokens.${tokenSymbol}`
                );
                if (tokenAddress === ZERO_ADDRESS) {
                    throw new Error("Underlying ERC20 Token not found");
                }
                const tokenInfo = await sf.contracts.TokenInfo.at(tokenAddress);
                const tokenInfoName = await tokenInfo.name.call();
                const tokenInfoSymbol = await tokenInfo.symbol.call();
                const tokenInfoDecimals = await tokenInfo.decimals.call();
                console.log("Underlying token address", tokenAddress);
                console.log("Underlying token info name()", tokenInfoName);
                console.log("Underlying token info symbol()", tokenInfoSymbol);
                console.log(
                    "Underlying token info decimals()",
                    tokenInfoDecimals.toString()
                );
                superTokenKey = `supertokens.${protocolReleaseVersion}.${tokenSymbol}x`;
                deploymentFn = async () => {
                    return await sf.createERC20Wrapper(tokenInfo);
                };
            }
        }

        const superTokenAddress = await sf.resolver.get(superTokenKey);
        console.log("SuperToken key at the resolver: ", superTokenKey);

        console.log("SuperToken address: ", superTokenAddress);
        let doDeploy = false;
        if (resetToken || superTokenAddress == ZERO_ADDRESS) {
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
                const superTokenLogic1 =
                    await superTokenFactory.getSuperTokenLogic();
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
                    await sendGovernanceAction(sf, (gov) =>
                        gov.updateSuperTokenLogic(
                            sf.host.address,
                            superTokenAddress
                        )
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
                }
            }
        }
        if (doDeploy) {
            console.log("Creating the wrapper...");
            const superToken = await deploymentFn();
            console.log("Wrapper created at", superToken.address);
            console.log("Resolver setting new address...");
            const testResolver = await TestResolver.at(sf.resolver.address);
            await testResolver.set(superTokenKey, superToken.address);
            console.log("Resolver set done.");
        }

        console.log("======== Super token deployed ========");
        callback();
    } catch (err) {
        callback(err);
    }
};
