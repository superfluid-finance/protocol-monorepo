const _ = require("lodash");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const { detectTruffleAndConfigure, extractWeb3Options } = require("./utils");

async function fetchLatestChanges(contract, eventName, filter) {
    const changes = await contract.getPastEvents(eventName, {
        fromBlock: 0,
        toBlock: "latest",
        filter,
    });
    return Object.values(
        changes.reduce(
            (acc, i) =>
                _.merge(acc, {
                    [i.args.superToken]: i.args,
                }),
            {}
        )
    ).filter((i) => !!i.set);
}

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/show-protocol-info.js
 */
module.exports = async function (callback, argv, options = {}) {
    try {
        await eval(`(${detectTruffleAndConfigure.toString()})(options)`);

        const sf = new SuperfluidSDK.Framework({
            ...extractWeb3Options(options),
            version: process.env.RELEASE_VERSION || "test",
            additionalContracts: [
                "AccessControl",
                "Ownable",
                "UUPSProxiable",
                "Superfluid",
                "SuperTokenFactory",
                "SuperfluidGovernanceBase",
            ],
            loadSuperNativeToken: true,
        });
        await sf.initialize();

        console.log("===== Protocol Information =====");

        {
            console.log("# Resolver");
            console.log("address", sf.resolver.address);
            const ADMIN_ROLE = "0x" + "0".repeat(64);
            const ac = await sf.contracts.AccessControl.at(sf.resolver.address);
            const maybeMembers = Array.from(
                new Set([
                    ...(
                        await ac.getPastEvents("RoleGranted", {
                            fromBlock: 0,
                            toBlock: "latest",
                            role: ADMIN_ROLE,
                        })
                    ).map((i) => i.args.account),
                    ...(
                        await ac.getPastEvents("RoleRevoked", {
                            fromBlock: 0,
                            toBlock: "latest",
                            role: ADMIN_ROLE,
                        })
                    ).map((i) => i.args.account),
                ])
            );
            for (let i = 0; i < maybeMembers.length; ++i) {
                if (await ac.hasRole(ADMIN_ROLE, maybeMembers[i])) {
                    console.log("admin", i, maybeMembers[i]);
                }
            }
            console.log("\n");
        }

        let host;
        {
            console.log("# Host");
            host = await sf.contracts.Superfluid.at(sf.host.address);
            console.log("address", host.address);
            console.log(
                "code address",
                await (
                    await sf.contracts.UUPSProxiable.at(sf.host.address)
                ).getCodeAddress()
            );
            console.log(
                "NON_UPGRADABLE_DEPLOYMENT",
                await host.NON_UPGRADABLE_DEPLOYMENT.call()
            );
            console.log(
                "APP_WHITE_LISTING_ENABLED",
                (await host.APP_WHITE_LISTING_ENABLED.call()).toString()
            );
            console.log(
                "MAX_APP_LEVEL",
                (await host.MAX_APP_LEVEL.call()).toString()
            );
            console.log(
                "CALLBACK_GAS_LIMIT",
                (await host.CALLBACK_GAS_LIMIT.call()).toString()
            );
            console.log("\n");
        }

        let gov;
        {
            console.log("# Governance");
            gov = await sf.contracts.SuperfluidGovernanceBase.at(
                await sf.host.getGovernance.call()
            );
            console.log("address", gov.address);
            console.log(
                "owner",
                await (await sf.contracts.Ownable.at(gov.address)).owner.call()
            );
        }
        {
            console.log("## RewardAddress");
            const latests = await fetchLatestChanges(
                gov,
                "RewardAddressChanged",
                {
                    host: host.address,
                }
            );
            latests.forEach((i) => console.log(i.superToken, i.rewardAddress));
        }
        {
            console.log("## CFAv1LiquidationPeriod");
            const latests = await fetchLatestChanges(
                gov,
                "CFAv1LiquidationPeriodChanged",
                {
                    host: host.address,
                }
            );
            latests.forEach((i) =>
                console.log(i.superToken, i.liquidationPeriod.toString())
            );
        }
        {
            console.log("## TrustedForwarders");
            const latests = await fetchLatestChanges(
                gov,
                "TrustedForwarderChanged",
                {
                    host: host.address,
                }
            );
            latests
                .filter((i) => !!i.enabled)
                .forEach((i) => console.log(i.superToken, i.forwarder));
        }
        console.log("\n");

        console.log("# Listed SuperTokens");
        if (sf.config.nativeTokenSymbol) {
            console.log("## SuperToken of Native Chain Token");
            const token = sf.tokens[sf.config.nativeTokenSymbol + "x"];
            console.log(await token.symbol.call(), token.address);
            console.log(
                "Underlying Wrapped Native",
                await token.getUnderlyingToken.call()
            );
        }
        {
            console.log("## Other Unlisted SuperTokens");
            let superTokenFactory = await sf.contracts.SuperTokenFactory.at(
                await sf.host.getSuperTokenFactory()
            );
            const latests = await superTokenFactory.getPastEvents(
                "SuperTokenCreated",
                {
                    fromBlock: 0,
                    toBlock: "latest",
                }
            );
            for (let i = 0; i < latests.length; ++i) {
                const token = await sf.contracts.ISuperToken.at(
                    latests[i].args.token
                );
                console.log(
                    await token.symbol.call(),
                    token.address,
                    `(${await token.getUnderlyingToken.call()})`
                );
            }
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
