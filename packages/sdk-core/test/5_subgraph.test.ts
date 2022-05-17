import { Framework } from "../src";
import { setup } from "../scripts/setup";
import { expect } from "chai";

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
        const events = await framework.query.listEvents({ }, { take: 10 });
        expect(events.data.length).to.be.greaterThan(0);
    });
});
