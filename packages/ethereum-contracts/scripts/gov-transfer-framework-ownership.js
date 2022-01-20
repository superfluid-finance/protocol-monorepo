const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {web3tx} = require("@decentral.ee/web3-helpers");
const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
} = require("./libs/common");

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/gov-transfer-framework-ownership.js : {NEW_GOV_OWNER} [ {NEW_RESOLVER_ADMIN} ]
 *        if NEW_RESOLVER_ADMIN is not set, only governance ownership is changed
 *        set NEW_GOV_OWNER to the current owner in order to only set a new resolver admin
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let {protocolReleaseVersion} = options;

    if (args.length !== 1 && args.length !== 2) {
        throw new Error("Wrong number of arguments");
    }

    let newResolverAdmin;
    if (args.length === 2) {
        newResolverAdmin = args.pop();
    }
    const newGovOwner = args.pop();

    const myAccount = (await web3.eth.getAccounts())[0];
    console.log("My account", myAccount);
    console.log("Transfering ownership of the framework to", newGovOwner);
    if (newResolverAdmin !== undefined) {
        console.log(
            "Transfering admin role of the resolver to",
            newResolverAdmin
        );
    }

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: ["AccessControl", "Ownable"],
    });
    await sf.initialize();

    if (newGovOwner.toLowerCase() !== myAccount.toLowerCase()) {
        const govAddress = await sf.host.getGovernance.call();
        console.log("Governance address", govAddress);
        const ownable = await sf.contracts.Ownable.at(govAddress);
        if ((await ownable.owner()).toLowerCase() === myAccount.toLowerCase()) {
            await web3tx(
                ownable.transferOwnership,
                "Transfer the ownership of the governance to new owner"
            )(newGovOwner);
        } else {
            console.log("ERR: I am not owner of the governance.");
        }
    } else {
        console.log("I remain the owned of the governance.");
    }

    if (
        newResolverAdmin !== undefined &&
        newResolverAdmin.toLowerCase() !== myAccount.toLowerCase()
    ) {
        console.log("Resolver address", sf.resolver.address);
        const ADMIN_ROLE = "0x" + "0".repeat(64);
        const ac = await sf.contracts.AccessControl.at(sf.resolver.address);
        if (await ac.hasRole(ADMIN_ROLE, myAccount)) {
            await web3tx(
                ac.grantRole,
                "Grant admin rights of the resolver to new admin"
            )(ADMIN_ROLE, newResolverAdmin);
            await web3tx(
                ac.revokeRole,
                "Revoke my admin rights to the resolver"
            )(ADMIN_ROLE, myAccount);
        } else {
            console.log("ERR: I am not admin of the resolver.");
        }
    } else {
        console.log("I remain the admin of the resolver.");
    }
});
