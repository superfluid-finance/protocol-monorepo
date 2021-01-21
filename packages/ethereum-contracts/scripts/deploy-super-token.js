const Web3 = require("web3");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const loadContracts = require("./loadContracts");
const { parseColonArgs, ZERO_ADDRESS } = require("./utils");

/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 * @param isTruffle (optional) Whether the script is used within the truffle framework
 * @param web3Provider (optional) The web3 provider to be used instead
 * @param from (optional) Address to deploy contracts from, use accounts[0] by default
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : {TOKEN_NAME}
 */
module.exports = async function(
    callback,
    argv,
    { isTruffle, web3Provider, from } = {}
) {
    try {
        this.web3 = web3Provider ? new Web3(web3Provider) : web3;
        if (!this.web3) throw new Error("No web3 is available");

        if (!from) {
            const accounts = await this.web3.eth.getAccounts();
            from = accounts[0];
        }

        const {
            TestResolver,
            UUPSProxiable,
            ISuperfluidGovernance,
            ISuperToken
        } = loadContracts({
            isTruffle,
            web3Provider: this.web3.currentProvider,
            from
        });

        console.log("Deploying super token");

        const chainId = await this.web3.eth.net.getId(); // TODO use eth.getChainId;
        const version = process.env.RELEASE_VERSION || "test";
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();
        console.log("Underlying token name", tokenName);

        const sf = new SuperfluidSDK.Framework({
            isTruffle,
            web3Provider: this.web3.currentProvider,
            version,
            from
        });
        await sf.initialize();

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
                const superTokenFactory = await sf.contracts.ISuperTokenFactory.at(
                    await sf.host.getSuperTokenFactory.call()
                );
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
            const superToken = await sf.createERC20Wrapper(tokenInfo, { from });
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
