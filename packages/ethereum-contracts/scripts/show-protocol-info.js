const _ = require("lodash");
const async = require("async");
const getConfig = require("./getConfig");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const {
    ZERO_ADDRESS,
    detectTruffleAndConfigure,
    extractWeb3Options,
} = require("./utils");

const MAX_REQUESTS = 100;
let config;

async function getPastEvents(contract, eventName, filter) {
    if (!config.hack_disableGetLogs) {
        return await contract.getPastEvents(eventName, {
            fromBlock: 0,
            toBlock: "latest",
            filter,
        });
    } else {
        console.warn("[WARN] eth_getLogs disabled for", eventName);
        return [];
    }
}

async function fetchLatestChanges(contract, eventName, filter) {
    const changes = await getPastEvents(contract, eventName, {
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
                "SuperToken",
                "SuperfluidGovernanceBase",
            ],
            loadSuperNativeToken: true,
        });
        await sf.initialize();

        config = getConfig(sf.networkId);

        console.log("\n===== Protocol Information =====\n");

        let host;
        {
            console.log("# Host\n");
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
            console.log("");
        }

        let gov;
        {
            console.log("# Governance\n");
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
            console.log(
                "DEFAULT",
                await gov.getRewardAddress.call(host.address, ZERO_ADDRESS)
            );
            const latests = (
                await fetchLatestChanges(gov, "RewardAddressChanged", {
                    host: host.address,
                })
            ).filter((i) => i.superToken !== ZERO_ADDRESS);
            latests.forEach((i) => console.log(i.superToken, i.rewardAddress));
        }
        {
            console.log("## CFAv1LiquidationPeriod");
            console.log(
                "DEFAULT",
                (
                    await gov.getCFAv1LiquidationPeriod.call(
                        host.address,
                        ZERO_ADDRESS
                    )
                ).toString()
            );
            const latests = (
                await fetchLatestChanges(gov, "CFAv1LiquidationPeriodChanged", {
                    host: host.address,
                })
            ).filter((i) => i.superToken !== ZERO_ADDRESS);
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
        console.log("");

        console.log("# Super Token Factory\n");
        let superTokenFactory;
        let latestSuperTokenLogicAddress;
        {
            superTokenFactory = await sf.contracts.SuperTokenFactory.at(
                await sf.host.getSuperTokenFactory()
            );
            console.log("SuperTokenFactory address", superTokenFactory.address);
            console.log(
                "SuperTokenLogic address",
                await superTokenFactory.getSuperTokenLogic()
            );
            latestSuperTokenLogicAddress =
                await superTokenFactory.getSuperTokenLogic();
        }
        console.log("");

        console.log("# Managed Super Tokens\n");
        {
            const printSuperToken = (s) => {
                const needsUpdate =
                    s.superTokenLogicAddress.toLowerCase() !==
                    latestSuperTokenLogicAddress.toLowerCase();
                console.log(
                    `${s.name} (${s.symbol})`,
                    s.tokenAddress,
                    `(${s.underlyingTokenAddress})`,
                    needsUpdate ? `*(${s.superTokenLogicAddress})` : ""
                );
            };
            if (sf.config.nativeTokenSymbol) {
                console.log("## SuperToken of Native Chain Token");
                const superToken = await sf.contracts.SuperToken.at(
                    sf.tokens[sf.config.nativeTokenSymbol + "x"].address
                );
                const symbol = await superToken.symbol.call();
                const superTokenLogicAddress = await (
                    await sf.contracts.UUPSProxiable.at(superToken.address)
                ).getCodeAddress();
                printSuperToken({
                    symbol,
                    name: await superToken.name.call(),
                    tokenAddress: superToken.address,
                    superTokenLogicAddress,
                    underlyingTokenAddress:
                        await superToken.getUnderlyingToken.call(),
                });
            }
            console.log("");

            {
                const latests = [
                    ...(await getPastEvents(
                        superTokenFactory,
                        "CustomSuperTokenCreated"
                    )),
                    ...(await getPastEvents(
                        superTokenFactory,
                        "SuperTokenCreated"
                    )),
                ];
                const superTokens = await async.mapLimit(
                    latests,
                    MAX_REQUESTS,
                    async (pastEvent) => {
                        const superToken = await sf.contracts.SuperToken.at(
                            pastEvent.args.token
                        );
                        const symbol = await superToken.symbol.call();
                        const superTokenLogicAddress =
                            await superToken.getCodeAddress();
                        const isListed =
                            (
                                await sf.resolver.get.call(
                                    `supertokens.${sf.version}.${symbol}`
                                )
                            ).toLowerCase() == superToken.address.toLowerCase();
                        return {
                            symbol,
                            name: await superToken.name.call(),
                            tokenAddress: superToken.address,
                            superTokenLogicAddress,
                            underlyingTokenAddress:
                                await superToken.getUnderlyingToken.call(),
                            isListed,
                        };
                    }
                );

                console.log("## Listed Super Tokens");
                superTokens.filter((s) => s.isListed).forEach(printSuperToken);
                console.log("");

                console.log("## Unlisted Super Tokens");
                superTokens.filter((s) => !s.isListed).forEach(printSuperToken);
                console.log("");

                console.log("* - Needs super token logic update");
            }
            console.log("");
        }

        console.log("\n===== Resolver Information =====\n");

        {
            console.log("address", sf.resolver.address);
            const ADMIN_ROLE = "0x" + "0".repeat(64);
            const ac = await sf.contracts.AccessControl.at(sf.resolver.address);
            const maybeMembers = Array.from(
                new Set([
                    ...(
                        await getPastEvents(ac, "RoleGranted", {
                            role: ADMIN_ROLE,
                        })
                    ).map((i) => i.args.account),
                    ...(
                        await getPastEvents(ac, "RoleRevoked", {
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
            console.log("");
        }

        callback();
    } catch (err) {
        callback(err);
    }
};
