const fs = require("fs");
const {ethers} = require("hardhat");
const superTokenFactoryArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/superfluid/SuperTokenFactory.sol/SuperTokenFactory.json");
const superTokenArtifact = require("@superfluid-finance/ethereum-contracts/build/hardhat/contracts/superfluid/SuperToken.sol/SuperToken.json");
const {deployContractsAndToken} = require("./deploy-contracts-and-token");

deployContractsAndToken()
    .then(async ({deployer, tokenDeploymentOutput}) => {
        const frameworkAddresses = await deployer.getFramework();

        const superTokenFactory = await ethers.getContractAt(
            superTokenFactoryArtifact.abi,
            frameworkAddresses.superTokenFactory
        );
        const superTokenLogicAddress =
            await superTokenFactory.getSuperTokenLogic();
        const superTokenLogic = await ethers.getContractAt(
            superTokenArtifact.abi,
            superTokenLogicAddress
        );
        const constantOutflowNFTAddress =
            await superTokenLogic.CONSTANT_OUTFLOW_NFT();
        const constantInflowNFTAddress =
            await superTokenLogic.CONSTANT_INFLOW_NFT();

        const deploymentOutput = {
            network: "mainnet",
            testNetwork: "hardhat",
            hostStartBlock: 0,
            hostAddress: frameworkAddresses.host,
            cfaAddress: frameworkAddresses.cfa,
            idaAddress: frameworkAddresses.ida,
            superTokenFactoryAddress: frameworkAddresses.superTokenFactory,
            resolverV1Address: frameworkAddresses.resolver,
            nativeAssetSuperTokenAddress:
                tokenDeploymentOutput.nativeAssetSuperTokenData
                    .nativeAssetSuperTokenAddress,
            constantOutflowNFTAddress,
            constantInflowNFTAddress,
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
