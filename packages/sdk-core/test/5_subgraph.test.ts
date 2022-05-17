import { Framework } from "../src";
import { setup } from "../scripts/setup";

describe("Subgraph Tests", () => {
    let framework: Framework;

    before(async () => {
        const subgraphEndpoint =
            "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-matic";
        const { frameworkClass } = await setup({
            subgraphEndpoint,
        });
        framework = frameworkClass;
    });

    it("Should be able to make the getAllEvents query", async () => {
        const events = await framework.query.listEvents({});
        console.log(events);
    });
});
