/*
 * Usage: npx hardhat deploy --network <network>
 *
 * Notes:
 * You need to have a .env file based on .env-template.
 * If verification fails, you can run again this script to verify later.
 */

const metadata = require("@superfluid-finance/metadata");

const sleep = (waitTimeInMs) =>
    new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

module.exports = async function ({ deployments, getNamedAccounts }) {
    const chainId = await hre.getChainId();
    const host = metadata.networks.filter((item) => item.chainId == chainId)[0]
        .contractsV1.host;
    const registrationKey = "";
    if (host === undefined) {
        console.log("Host contract not found for this network");
        return;
    }

    const { deploy } = deployments;
    const { deployer } = await getNamedAccounts();

    console.log(`network: ${hre.network.name}`);
    console.log(`chainId: ${chainId}`);
    console.log(`rpc: ${hre.network.config.url}`);
    console.log(`host: ${host}`);

    const FlowScheduler = await deploy("FlowScheduler", {
        from: deployer,
        args: [host, registrationKey],
        log: true,
        skipIfAlreadyDeployed: false,
    });

    const VestingScheduler = await deploy("VestingScheduler", {
        from: deployer,
        args: [host, registrationKey],
        log: true,
        skipIfAlreadyDeployed: false,
    });

    // wait for 15 seconds to allow etherscan to indexed the contracts
    await sleep(15000);

    try {
        await hre.run("verify:verify", {
            address: FlowScheduler.address,
            constructorArguments: [host, registrationKey],
            contract: "contracts/FlowScheduler.sol:FlowScheduler",
        });

        await hre.run("verify:verify", {
            address: VestingScheduler.address,
            constructorArguments: [host, registrationKey],
            contract: "contracts/VestingScheduler.sol:VestingScheduler",
        });
    } catch (err) {
        console.error(err);
    }
};
