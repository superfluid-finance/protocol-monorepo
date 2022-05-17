import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";
import { SuperToken as SuperTokenType } from "../src/typechain";
import { Framework, WrapperSuperToken } from "../src";
import { ROPSTEN_SUBGRAPH_ENDPOINT } from "./0_framework.test";
import { setup } from "../scripts/setup";

describe.only("Subgraph Tests", () => {
    let framework: Framework;
    let deployer: SignerWithAddress;
    let superToken: SuperTokenType;
    let daix: WrapperSuperToken;

    before(async () => {
        const subgraphEndpoint =
            "https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-matic";
        const { frameworkClass, Deployer, SuperToken } = await setup({
            subgraphEndpoint,
        });
        framework = frameworkClass;
        deployer = Deployer;
        superToken = SuperToken;
        daix = await framework.loadWrapperSuperToken(superToken.address);
    });

    it("Should be able to make the getAllEvents query", async () => {
        const events = await framework.query.listEvents({});
        console.log(events);
    });
});
