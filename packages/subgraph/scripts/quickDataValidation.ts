import { getAccountTokenSnapshots } from "./dataIntegrity/dataIntegrityQueries";
import {
    getMostRecentIndexedBlockNumber,
    QueryHelper,
} from "./dataIntegrity/helperFunctions";
import { DataIntegrityAccountTokenSnapshot } from "./dataIntegrity/interfaces";

async function main() {
    const endpointA =
        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-feature-goerli";
    const endpointB =
        "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-goerli";
    const endpointARecentBlock = await getMostRecentIndexedBlockNumber(
        endpointA
    );
    const endpointAQueryHelper = new QueryHelper(
        endpointA,
        endpointARecentBlock,
        1000
    );
    const endpointBRecentBlock = await getMostRecentIndexedBlockNumber(
        endpointB
    );
    const endpointBQueryHelper = new QueryHelper(
        endpointB,
        endpointBRecentBlock,
        1000
    );

    console.log("Retrieving Feature Endpoint data");
    const endpointAData =
        await endpointAQueryHelper.getAllResults<DataIntegrityAccountTokenSnapshot>(
            {
                query: getAccountTokenSnapshots,
                isUpdatedAt: true,
            }
        );

    console.log("Retrieving V1 Endpoint data");
    const endpointBData =
        await endpointBQueryHelper.getAllResults<DataIntegrityAccountTokenSnapshot>(
            {
                query: getAccountTokenSnapshots,
                isUpdatedAt: true,
            }
        );
    console.log("Validating Data");

    if (endpointAData.length !== endpointBData.length) {
        throw new Error("Different length results");
    }

    let errors = 0;
    for (let i = 0; i < endpointAData.length; i++) {
        if (
            endpointAData[i].balanceUntilUpdatedAt !==
            endpointBData[i].balanceUntilUpdatedAt
        ) {
            console.log("FEATURE:", JSON.stringify(endpointAData[i]));
            console.log("V1:", JSON.stringify(endpointBData[i]));
            errors++;
        }
    }

    if (errors === 0) console.log("Success");
}

main().then(() => {
    process.exit(0);
});
