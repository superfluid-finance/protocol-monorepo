const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { web3tx } = require("@decentral.ee/web3-helpers");
const {
    detectTruffleAndConfigure,
    parseColonArgs,
    extractWeb3Options,
} = require("./utils");

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/transfer-framework-ownwership.js : new_owner
 */

module.exports = async function (callback, argv, options = {}) {
    try {
        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        const args = parseColonArgs(argv || process.argv);
        if (args.length != 1) {
            throw new Error("Not enough arguments");
        }
        const newOwner = args.shift();

        const myAccount = (await web3.eth.getAccounts())[0];
        console.log("My account", myAccount);
        console.log("Transfering ownership of the framework to", newOwner);

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: process.env.RELEASE_VERSION || "test",
            additionalContracts: ["AccessControl", "Ownable"],
        });
        await sf.initialize();

        {
            console.log("Resolver address", sf.resolver.address);
            const ADMIN_ROLE = "0x" + "0".repeat(64);
            const ac = await sf.contracts.AccessControl.at(sf.resolver.address);
            if (await ac.hasRole(ADMIN_ROLE, myAccount)) {
                await web3tx(
                    ac.grantRole,
                    "Grant admin rights of the resolver to new owner"
                )(ADMIN_ROLE, newOwner);
                await web3tx(
                    ac.revokeRole,
                    "Revoke my admin rights to the resolver"
                )(ADMIN_ROLE, myAccount);
            } else {
                console.log("I am not admin of the resolver.");
            }
        }

        {
            const govAddress = await sf.host.getGovernance.call();
            console.log("Governance address", govAddress);
            const ownable = await sf.contracts.Ownable.at(govAddress);
            if (
                (await ownable.owner()).toLowerCase() ===
                myAccount.toLowerCase()
            ) {
                await web3tx(
                    ownable.transferOwnership,
                    "Transfer the ownership of the governance to new owner"
                )(newOwner);
            } else {
                console.log("I am not owner of the governance.");
            }
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
