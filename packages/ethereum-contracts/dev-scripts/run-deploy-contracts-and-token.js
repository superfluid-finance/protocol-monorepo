const {Framework} = require("@superfluid-finance/sdk-core");
const fs = require("fs");
const {ethers} = require("hardhat");
const {deployContractsAndToken} = require("./deploy-contracts-and-token");

deployContractsAndToken()
    .then(async (deployer) => {
        const frameworkAddresses = await deployer.getFramework();

        const framework = await Framework.create({
            chainId: ethers.provider.network.chainId,
            resolverAddress: frameworkAddresses.resolver,
            provider: ethers.provider,
            protocolReleaseVersion: "test",
        });

        // get native asset super token address
        const nativeAssetSuperToken = await framework.loadNativeAssetSuperToken(
            "ETHx"
        );
        const deploymentOutput = {
            network: "mainnet",
            testNetwork: "hardhat",
            hostStartBlock: 0,
            hostAddress: frameworkAddresses.host,
            cfaAddress: frameworkAddresses.cfa,
            idaAddress: frameworkAddresses.ida,
            superTokenFactoryAddress: frameworkAddresses.superTokenFactory,
            resolverV1Address: frameworkAddresses.resolver,
            nativeAssetSuperTokenAddress: nativeAssetSuperToken.address,
        };

        // create json output
        const JSONOutput = JSON.stringify(deploymentOutput);

        // console addresses
        delete deploymentOutput.nativeAssetSuperTokenAddress;
        console.log("Framework Deployment Output:", deploymentOutput);

        // write to hardhat.json in packages/subgraph for local testing
        const writeToDir =
            __dirname.split("ethereum-contracts")[0] +
            "subgraph/config/hardhat.json";
        fs.writeFile(writeToDir, JSONOutput, (err) => {
            if (err) {
                console.error(err);
                process.exit(1);
            } else {
                process.exit(0);
            }
        });
    })
    .catch((err) => {
        console.error(err);
        process.exit(1);
    });
