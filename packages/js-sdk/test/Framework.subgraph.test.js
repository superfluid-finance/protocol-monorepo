const path = require("path");
const Web3 = require("web3");
const HDWalletProvider = require("@truffle/hdwallet-provider");
const SuperfluidSDK = require("../src");

describe("Framework subgraph (goerli) support", function () {
    this.timeout(300e3);

    let sf;

    before(async function () {
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
            version: "v1",
        });
        await sf.initialize();
    });

    it("subgraphQuery", async () => {
        await sf.subgraphQuery({});
    });
});
