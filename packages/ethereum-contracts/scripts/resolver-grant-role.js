const {web3tx} = require("@decentral.ee/web3-helpers");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const getConfig = require("./libs/getConfig");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
    builtTruffleContractLoader,
} = require("./libs/common");

/**
 * @dev Grant a role to an account
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to be used as sender
 *
 * Usage: npx truffle exec scripts/resolver-grant-role.js : {ACCOUNT} [ {ROLE} ]
 *    ACCOUNT is the account you want to grant a role
 *    ROLE (optional) is the role to be granted, defaults to the ADMIN_ROLE
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    console.log("======== Resolver: grant role ========");

    if (args.length !== 1 && args.length !== 2) {
        throw new Error("Wrong number of arguments (or missing colon)");
    }

    const DEFAULT_ADMIN_ROLE = "0x0";

    let role = DEFAULT_ADMIN_ROLE;
    if (args.length === 2) {
        role = args.pop();
    }
    const account = args.pop();
    console.log("Account", account);
    console.log("Role", role);

    const chainId = await web3.eth.getChainId();
    const config = getConfig(chainId);

    const {Resolver} = await SuperfluidSDK.loadContracts({
        ...extractWeb3Options(options),
        additionalContracts: ["Resolver"],
        contractLoader: builtTruffleContractLoader,
    });

    const resolver = await Resolver.at(config.resolverAddress);

    const myAccount = (await web3.eth.getAccounts())[0];
    if (!(await resolver.hasRole(DEFAULT_ADMIN_ROLE, myAccount))) {
        console.error(
            `Sender account ${myAccount} doesn't have admin role on Resolver at ${resolver.address}`
        );
        return;
    }

    await web3tx(resolver.grantRole, "Grant role to account")(role, account);

    console.log("======== Role granted ======");
});
