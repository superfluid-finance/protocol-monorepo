import {Framework} from "@superfluid-finance/sdk-core";
import fs from "fs";
import {ethers} from "hardhat";
import {deployContractsAndToken} from "./deployContractsAndToken";

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

        // create json output
        const JSONOutput = JSON.stringify({
            network: "mainnet",
            testNetwork: "hardhat",
            hostStartBlock: 0,
            hostAddress: frameworkAddresses.host,
            cfaAddress: frameworkAddresses.cfa,
            idaAddress: frameworkAddresses.ida,
            superTokenFactoryAddress: frameworkAddresses.superTokenFactory,
            resolverV1Address: frameworkAddresses.resolver,
            nativeAssetSuperTokenAddress: nativeAssetSuperToken.address,
        });

        // write to hardhat.json in packages/subgraph for local testing
        const writeToDir =
            __dirname.split("ethereum-contracts")[0] +
            "subgraph/config/hardhat.json";
        fs.writeFile(writeToDir, JSONOutput, (err: any) => {
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
