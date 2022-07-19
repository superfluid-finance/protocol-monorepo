const {
    getScriptRunnerFactory: S,
    extractWeb3Options,
} = require("./libs/common");

const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const SuperfluidGovernanceIIProxy = artifacts.require(
    "SuperfluidGovernanceIIProxy"
);
const SuperfluidGovernanceII = artifacts.require("SuperfluidGovernanceII");
const TokenCustodian = artifacts.require("TokenCustodian");
const TOGA = artifacts.require("TOGA");

const BatchLiquidator = artifacts.require("BatchLiquidator");

/**
 * @dev Deploy auxiliary contracts: governance, TOGA, BatchLiquidator
 * To be used after a community mainnet deployment,
 * with the "handover account" set as message sender.
 * - deploy SuperfluidGovernanceII with proxy
 * - set 3Ps config
 * - deploy TOGA
 * - set TOGA as rewardAddress
 * - deploy BatchLiquidator
 *
 * @param web3 The web3 instance to be used
 * @param from address to use for funding the deployer account
 *
 * Usage: npx truffle exec scripts/deploy-aux-contracts.js
 *
 * (optional) ENV vars:
 * - GOV_LIQUIDATION_PERIOD overrides the default value of 4 hours
 * - GOV_PATRICIAN_PERIOD overrides the default value of 30 minutes
 * - TOGA_MIN_BOND_DURATION overrides the default value of 7 days
 *
 * You may also need to set RESOLVER_ADDRESS if not yet set in js-sdk.
 */
module.exports = eval(`(${S.toString()})()`)(async function (
    args,
    options = {}
) {
    let {protocolReleaseVersion} = options;

    console.log("======== Deploying auxiliary contracts ========");

    const sf = new SuperfluidSDK.Framework({
        ...extractWeb3Options(options),
        version: protocolReleaseVersion,
    });
    await sf.initialize();

    console.log("deploying production governance...");
    const govProxy = await SuperfluidGovernanceIIProxy.new();
    console.log("governance proxy deployed at:", govProxy.address);
    const govLogic = await SuperfluidGovernanceII.new();
    console.log("governance logic deployed at:", govLogic.address);
    await govLogic.castrate();
    console.log("marked gov logic as initialized (castrate)");
    await govProxy.initializeProxy(govLogic.address);
    console.log("governance proxy initialized with logic");
    const gov = await SuperfluidGovernanceII.at(govProxy.address);

    const liquidationPeriod = process.env.GOV_LIQUIDATION_PERIOD || 14400;
    const patricianPeriod = process.env.GOV_PATRICIAN_PERIOD || 1800;
    await gov.setPPPConfig(
        sf.host.address,
        "0x0000000000000000000000000000000000000000", // default - for all tokens
        liquidationPeriod,
        patricianPeriod
    );
    console.log(
        `set 3Ps config to liquidation period of ${liquidationPeriod} and patrician period of ${patricianPeriod}`
    );

    console.log("deploying solvency related contracts");
    const tokenCustodian = await TokenCustodian.new();
    console.log("TokenCustodian deployed at: ", tokenCustodian.address);
    const minBondDuration = process.env.TOGA_MIN_BOND_DURATION || 604800;
    const toga = await TOGA.new(
        sf.host.address,
        minBondDuration,
        tokenCustodian.address
    );
    console.log("TOGA deployed at:", toga.address);

    await gov.setRewardAddress(
        sf.host.address,
        "0x0000000000000000000000000000000000000000",
        toga.address
    );
    console.log("reward address set to TOGA");

    const batchLiquidator = await BatchLiquidator.new();
    console.log("BatchLiquidator deployed at:", batchLiquidator.address);

    console.log("");
    console.log("LEFT TO BE DONE MANUALLY:");
    console.log("replace old governance with new governance:");
    console.log(
        `gov.replaceGovernance(${sf.host.address}, ${govProxy.address})`
    );
});
