const path = require("path");
const Web3 = require("web3");
const HDWalletProvider = require("@truffle/hdwallet-provider");
const SuperfluidSDK = require("../src");

describe("Framework subgraph (matic) support", function () {
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
                    process.env.POLYGON_MAINNET_PROVIDER_URL ||
                        // use a default one for public pull requests
                        "https://polygon-rpc.com/",
                    0, //address_index
                    1 // num_addresses
                )
            ),
            tokens: ["DAIx", "ETHx"],
            version: "v1",
        });
        await sf.initialize();
    });

    // Taking an address from https://docs.ricochet.exchange/docs/network-directory:
    // DAI >> WETH
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
            superToken: sf.tokens.DAIx.address,
            account: "0x9BEf427fa1fF5269b824eeD9415F7622b81244f5",
        });
        assert.isTrue(flows.inFlows.length > 0);
    });

    it("ida.listIndices", async () => {
        const indexes = await sf.ida.listIndices({
            superToken: sf.tokens.ETHx.address,
            publisher: "0x9BEf427fa1fF5269b824eeD9415F7622b81244f5",
        });
        assert.equal(indexes.length, 1);
        assert.equal(indexes[0], 0);
    });

    it("ida.listSubscribers", async () => {
        const subscribers = await sf.ida.listSubscribers({
            superToken: sf.tokens.ETHx.address,
            publisher: "0x9BEf427fa1fF5269b824eeD9415F7622b81244f5",
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
                token: sf.tokens.ETHx.address,
                publisher: "0x9BEf427fa1fF5269b824eeD9415F7622b81244f5",
                indexId: 0,
            }
        );
        assert.isTrue(events.length > 0);
        assert.equal(
            events[0].token.toLowerCase(),
            sf.tokens.ETHx.address.toLowerCase()
        );
        assert.equal(
            events[0].publisher.toLowerCase(),
            "0x9BEf427fa1fF5269b824eeD9415F7622b81244f5".toLowerCase()
        );
        assert.equal(events[0].indexId, 0);
        assert.equal(events[0].userData, "0x");
    });
});
