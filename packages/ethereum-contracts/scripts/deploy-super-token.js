const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const loadContracts = require("./loadContracts");
const { parseColonArgs, ZERO_ADDRESS } = require("./utils");

/**
 * @dev Deploy test token (Mintable ERC20) to the network.
 * @param from address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/deploy-super-token.js : {TOKEN_NAME}
 */
module.exports = async function(
    callback,
    argv,
    { isTruffle, web3Provider, from } = {}
) {
    try {
        global.web3 = web3;

        if (!from) {
            const accounts = await web3.eth.getAccounts();
            from = accounts[0];
        }

        const { TestResolver, ISuperToken } = loadContracts({
            isTruffle,
            web3Provider: web3Provider || web3.currentProvider,
            from
        });

        const chainId = await web3.eth.net.getId(); // TODO use eth.getChainId;
        const version = process.env.RELEASE_VERSION || "test";
        console.log("network ID: ", chainId);
        console.log("release version:", version);

        const args = parseColonArgs(argv || process.argv);
        if (args.length !== 1) {
            throw new Error("Not enough arguments");
        }
        const tokenName = args.pop();

        const sf = new SuperfluidSDK.Framework({
            isTruffle,
            web3Provider: web3Provider || web3.currentProvider,
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
