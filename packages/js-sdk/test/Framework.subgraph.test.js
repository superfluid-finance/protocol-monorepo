const path = require("path");
const Web3 = require("web3");
const HDWalletProvider = require("@truffle/hdwallet-provider");
const SuperfluidSDK = require("../src");

describe("Framework subgraph (goerli) support", function () {
    this.timeout(300e3);

    let sf;

    before(async function () {
        // make sure no test resolver passed to the testsuite
        delete process.env.RESOLVER_ADDRESS;

        try {
            require("dotenv").config({
                path: path.join(__dirname, "..", ".env"),
            });
        } catch (e) {
            // no-empty
        }
        sf = new SuperfluidSDK.Framework({
            isTruffle: false,
            web3: new Web3(
                new HDWalletProvider(
                    "candy maple cake sugar pudding cream honey rich smooth crumble sweet treat",
                    process.env.GOERLI_PROVIDER_URL,
                    0, //address_index
                    1 // num_addresses
                )
            ),
            tokens: ["fDAIx"],
            version: "v1",
        });
        await sf.initialize();
    });

    it("subgraphQuery", async () => {
        const data = await sf.subgraphQuery(`{
            _meta {
                block {
                    number
                }
                deployment
            }
        }`);
        assert.isDefined(data._meta.block.number);
        assert.isDefined(data._meta.deployment);
    });

    it("getPastEvents SuperTokenLogicUpdated", async () => {
        const events = await sf.getPastEvents(
            sf.host,
            "SuperTokenLogicUpdated"
        );
        assert.isTrue(events.length > 0);
        assert.isDefined(events[0].token);
        assert.isDefined(events[0].code);
    });

    it("cfa.listFlows", async () => {
        const flows = await sf.cfa.listFlows({
            superToken: sf.tokens.fDAIx.address,
            account: "0xEb85888b31FADF79CB264d065EdcB4a14551c28d",
        });
        assert.isTrue(flows.inFlows.length > 0);
        assert.isTrue(flows.outFlows.length > 0);
    });

    it("ida.listIndices", async () => {
        const indexes = await sf.ida.listIndices({
            superToken: sf.tokens.fDAIx.address,
            publisher: "0x39aA80Fc05eD0b3549be279589Fc67f06b7e35EE",
        });
        assert.equal(indexes.length, 1);
        assert.equal(indexes[0], 0);
    });

    it("ida.listSubcribers", async () => {
        const subscribers = await sf.ida.listSubcribers({
            superToken: sf.tokens.fDAIx.address,
            publisher: "0x39aA80Fc05eD0b3549be279589Fc67f06b7e35EE",
            indexId: 0,
        });
        assert.isTrue(subscribers.length > 0);
        assert.isDefined(subscribers[0].subscriber);
        assert.isDefined(subscribers[0].units);
    });

    it("getPastEvents IndexCreated", async () => {
        const events = await sf.getPastEvents(
            sf.agreements.ida,
            "IndexCreated",
            {
                token: sf.tokens.fDAIx.address,
                publisher: "0x39aA80Fc05eD0b3549be279589Fc67f06b7e35EE",
                indexId: 0,
            }
        );
        assert.isTrue(events.length > 0);
        assert.equal(
            events[0].token.toLowerCase(),
            sf.tokens.fDAIx.address.toLowerCase()
        );
        assert.equal(
            events[0].publisher.toLowerCase(),
            "0x39aA80Fc05eD0b3549be279589Fc67f06b7e35EE".toLowerCase()
        );
        assert.equal(events[0].indexId, 0);
        assert.equal(events[0].userData, "0x");
    });
});
