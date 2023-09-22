import fs from "fs";
import metadata from "@superfluid-finance/metadata";

interface SubgraphConfig {
    readonly network: string;
    readonly hostStartBlock: number;
    readonly hostAddress: string;
    readonly cfaAddress: string;
    readonly idaAddress: string;
    readonly superTokenFactoryAddress: string;
    readonly resolverV1Address: string;
    readonly nativeAssetSuperTokenAddress: string;
}

// script usage: npx ts-node ./scripts/buildNetworkConfig.ts <NETWORK_NAME>
function main() {
    const networkName = process.argv[2];

    const networkMetadata = metadata.getNetworkByName(networkName);

    if (!networkMetadata) {
        throw new Error("No metadata found");
    }
    const newThing: SubgraphConfig = {
        network: networkMetadata.shortName,
        hostStartBlock: networkMetadata.startBlockV1,
        hostAddress: networkMetadata.contractsV1.host,
        cfaAddress: networkMetadata.contractsV1.cfaV1,
        idaAddress: networkMetadata.contractsV1.idaV1,
        superTokenFactoryAddress: networkMetadata.contractsV1.superTokenFactory,
        resolverV1Address: networkMetadata.contractsV1.resolver,
        nativeAssetSuperTokenAddress: networkMetadata.nativeTokenWrapper,
    };

    const writeToDir =
        __dirname.split("subgraph")[0] + `subgraph/config/${networkName}.json`;

    fs.writeFile(writeToDir, JSON.stringify(newThing), (err) => {
        if (err) {
            console.log(err);
            process.exit(1);
        } else {
            process.exit(0);
        }
    });
}

main();
