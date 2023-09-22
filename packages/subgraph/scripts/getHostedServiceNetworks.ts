import fs from "fs";
import metadata from "@superfluid-finance/metadata";

// This script is used to retrieve the list of networks which have a hosted
// service endpoint.
// script usage: npx ts-node ./scripts/getHostedServiceNetworks.ts
function main() {
    const networks = JSON.stringify(
        metadata.networks
            .filter((x) => x.subgraphV1.hostedEndpoint != null)
            .map((x) => x.name)
    );

    const writeToDir =
        __dirname.split("subgraph")[0] +
        "subgraph/hosted-service-networks.json";

    fs.writeFile(writeToDir, networks, (err) => {
        if (err) {
            console.log(err);
            process.exit(1);
        } else {
            process.exit(0);
        }
    });
}

main();
