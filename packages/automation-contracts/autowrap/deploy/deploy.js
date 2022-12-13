/*
 * Usage: npx hardhat deploy --network <network>
 *
 * Notes:
 * You need to have a .env file based on .env-example.
 * If verification fails, you can run again this script to verify later.
 */

const metadata = require("@superfluid-finance/metadata");

const sleep = (waitTimeInMs) =>
    new Promise((resolve) => setTimeout(resolve, waitTimeInMs));

module.exports = async function ({ deployments, getNamedAccounts }) {
    const chainId = await hre.getChainId();
    const cfaV1 = metadata.networks.filter((item) => item.chainId == chainId)[0]
        .contractsV1.cfaV1;
    if (cfaV1 === undefined) {
        console.log("cfaV1 contract not found for this network");
        return;
    }

    const minLower = 172800;
    const minUpper = 604800;

    const { deploy } = deployments;
    const { deployer } = await getNamedAccounts();

    console.log(`network: ${hre.network.name}`);
    console.log(`chainId: ${chainId}`);
    console.log(`rpc: ${hre.network.config.url}`);
    console.log(`cfaV1: ${cfaV1}`);

    const Manager = await deploy("Manager", {
        from: deployer,
        args: [cfaV1, minLower, minUpper],
        log: true,
        skipIfAlreadyDeployed: false,
    });

    const WrapStrategy = await deploy("WrapStrategy", {
        from: deployer,
        args: [Manager.address],
        log: true,
        skipIfAlreadyDeployed: false,
    });

    // approve strategy on manager contract
    await hre.run("addStrategy", {
        manager: Manager.address,
        strategy:WrapStrategy.address,
    });

    // wait for 15 seconds to allow etherscan to indexed the contracts
    await sleep(15000);

    try {
        await hre.run("verify:verify", {
            address: Manager.address,
            constructorArguments: [cfaV1, minLower, minUpper],
            contract: "contracts/Manager.sol:Manager",
        });

        await hre.run("verify:verify", {
            address: WrapStrategy.address,
            constructorArguments: [Manager.address],
            contract: "contracts/strategies/WrapStrategy.sol:WrapStrategy",
        });
    } catch (err) {
        console.error(err);
    }
};
