const _ = require("lodash");
const async = require("async");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const getConfig = require("./libs/getConfig");
const {
    getScriptRunnerFactory: S,
    ZERO_ADDRESS,
    extractWeb3Options,
} = require("./libs/common");

const MAX_REQUESTS = 100;

async function printHostInformation({sf}) {
    let host;
    console.log("# Host\n");
    try {
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
    } catch {
        console.error("Error printing host information.");
    }
    return {host};
}

async function printGovernanceInformation({sf}) {
    const fetchLatestGovernanceUpdate = async (contract, eventName, filter) => {
        const changes = await sf.getPastEvents(contract, eventName, filter);

        return Object.values(
            changes.reduce(
                (acc, i) =>
                    _.merge(acc, {
                        [i.superToken]: i,
                    }),
                {}
            )
        ).filter((i) => !!i.isKeySet);
    };

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
            await gov.getRewardAddress.call(sf.host.address, ZERO_ADDRESS)
        );
        try {
            const latests = (
                await fetchLatestGovernanceUpdate(gov, "RewardAddressChanged", {
                    host: sf.host.address,
                })
            ).filter((i) => i.superToken !== ZERO_ADDRESS);
            latests.forEach((i) =>
                console.log(
                    "SuperToken: " + i.superToken,
                    "\nReward Address: " + i.rewardAddress,
                    "\n---"
                )
            );
        } catch (e) {
            console.warn(
                "There was an error fetching RewardAddressChanged past events.",
                e
            );
        }
    }
    {
        console.log("## PPPConfiguration");
        const defaultPPPConfiguration = await gov.getPPPConfig(
            sf.host.address,
            ZERO_ADDRESS
        );
        console.log(
            "DEFAULT",
            "\nLiquidation Period: " +
                defaultPPPConfiguration.liquidationPeriod,
            "\nPatrician Period: " + defaultPPPConfiguration.patricianPeriod
        );
        try {
            const latests = (
                await fetchLatestGovernanceUpdate(
                    gov,
                    "PPPConfigurationChanged",
                    {
                        host: sf.host.address,
                    }
                )
            ).filter((i) => i.superToken !== ZERO_ADDRESS);
            latests.forEach((i) =>
                console.log(
                    "SuperToken: " + i.superToken,
                    "\nLiquidation Period: " + i.liquidationPeriod,
                    "\nPatrician Period: " + i.patricianPeriod,
                    "\n---"
                )
            );
        } catch (e) {
            console.warn(
                "There was an error fetching PPPConfigurationChanged past events.",
                e
            );
        }
    }
    {
        console.log("## TrustedForwarders");
        try {
            const latests = await fetchLatestGovernanceUpdate(
                gov,
                "TrustedForwarderChanged",
                {
                    host: sf.host.address,
                }
            );
            latests
                .filter((i) => !!i.enabled)
                .forEach((i) =>
                    console.log(
                        "SuperToken: " + i.superToken,
                        "\nForwarder: " + i.forwarder,
                        "\n---"
                    )
                );
        } catch (e) {
            console.warn(
                "There was an error fetching TrustedForwarderChanged past events.",
                e
            );
        }
    }
    return {gov};
}

async function printSuperTokenFactoryInformation({sf}) {
    let superTokenFactory, latestSuperTokenLogicAddress;

    superTokenFactory = await sf.contracts.SuperTokenFactory.at(
        await sf.host.getSuperTokenFactory()
    );
    console.log("SuperTokenFactory address", superTokenFactory.address);
    console.log(
        "SuperTokenLogic address",
        await superTokenFactory.getSuperTokenLogic()
    );
    latestSuperTokenLogicAddress = await superTokenFactory.getSuperTokenLogic();

    return {
        superTokenFactory,
        latestSuperTokenLogicAddress,
    };
}

async function printResolverInformation({sf}) {
    console.log("address", sf.resolver.address);
    const ADMIN_ROLE = "0x" + "0".repeat(64);
    const ac = await sf.contracts.AccessControl.at(sf.resolver.address);
    try {
        const maybeMembers = Array.from(
            new Set([
                ...(
                    await sf.getPastEvents(ac, "RoleGranted", {
                        role: ADMIN_ROLE,
                    })
                ).map((i) => i.account),
                ...(
                    await sf.getPastEvents(ac, "RoleRevoked", {
                        role: ADMIN_ROLE,
                    })
                ).map((i) => i.account),
            ])
        );
        for (let i = 0; i < maybeMembers.length; ++i) {
            if (await ac.hasRole(ADMIN_ROLE, maybeMembers[i])) {
                console.log("admin", i, maybeMembers[i]);
            }
        }
    } catch (e) {
        console.error("There was an error printing resolver information:", e);
    }
    console.log("");
}

async function printSuperTokensInformation({
    sf,
    superTokenFactory,
    latestSuperTokenLogicAddress,
}) {
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
        try {
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
        } catch {
            console.warn(`[WARN] SuperToken@${superToken.address} is smelly.`);
        }
    }
    console.log("");

    {
        const latests = [
            ...(await sf.getPastEvents(
                superTokenFactory,
                "CustomSuperTokenCreated"
            )),
            ...(await sf.getPastEvents(superTokenFactory, "SuperTokenCreated")),
        ];
        const superTokens = await async.mapLimit(
            latests,
            MAX_REQUESTS,
            async (pastEvent) => {
                const superToken = await sf.contracts.SuperToken.at(
                    pastEvent.token
                );
                try {
                    const symbol = await superToken.symbol.call();
                    const superTokenLogicAddress =
                        await superToken.getCodeAddress();
                    const isListed =
                        (
                            await sf.resolver.get.call(
                                `supertokens.${sf.version}.${symbol}`
                            )
                        ).toLowerCase() === superToken.address.toLowerCase();
                    return {
                        symbol,
                        name: await superToken.name.call(),
                        tokenAddress: superToken.address,
                        superTokenLogicAddress,
                        underlyingTokenAddress:
                            await superToken.getUnderlyingToken.call(),
                        isListed,
                    };
                } catch (e) {
                    console.error(
                        `Querying SuperToken at ${pastEvent.token} failed, may be a broken token deployment:\n ${e}\n`
                    );
                }
            }
        );

        console.log("## Listed Super Tokens");
        superTokens
            .filter((s) => s !== undefined && s.isListed)
            .filter((s) => s.superTokenLogicAddress !== ZERO_ADDRESS) // unintialized proxy
            .forEach(printSuperToken);
        console.log("");

        console.log("## Unlisted Super Tokens");
        superTokens
            .filter((s) => s !== undefined && !s.isListed)
            .filter((s) => s.superTokenLogicAddress !== ZERO_ADDRESS) // unintialized proxy
            .forEach(printSuperToken);
        console.log("");

        console.log("* - Needs super token logic update");
    }
    console.log("");
}

/**
 * @dev Inspect accounts and their agreements
 * @param {Array} argv Overriding command line arguments
 * @param {boolean} options.isTruffle Whether the script is used within native truffle framework
 * @param {Web3} options.web3  Injected web3 instance
 * @param {Address} options.from Address to deploy contracts from
 *
 * Usage: npx truffle exec scripts/info-show-protocol-info.js
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let {protocolReleaseVersion} = options;

    const networkType = await web3.eth.net.getNetworkType();
    const networkId = await web3.eth.net.getId();
    const chainId = await web3.eth.getChainId();
    console.log("network Type: ", networkType);
    console.log("network ID: ", networkId);
    console.log("chain ID: ", chainId);
    const config = getConfig(chainId);

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
        additionalContracts: [
            "AccessControl",
            "Ownable",
            "UUPSProxiable",
            "Superfluid",
            "SuperTokenFactory",
            "SuperToken",
            "SuperfluidGovernanceBase",
        ],
        tokens: config.tokenList,
        loadSuperNativeToken: true,
    });
    await sf.initialize();

    console.log("\n===== Protocol Information =====\n");

    await printHostInformation({sf});
    console.log("");

    await printGovernanceInformation({sf});
    console.log("");

    console.log("# Super Token Factory\n");
    const {superTokenFactory, latestSuperTokenLogicAddress} =
        await printSuperTokenFactoryInformation({sf});
    console.log("");

    console.log("# Managed Super Tokens\n");
    await printSuperTokensInformation({
        sf,
        superTokenFactory,
        latestSuperTokenLogicAddress,
    });

    console.log("\n===== Resolver Information =====\n");
    await printResolverInformation({sf});
});
