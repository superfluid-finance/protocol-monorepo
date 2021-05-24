const SuperfluidSDK = require("@superfluid-finance/js-sdk");

const {
    web3tx,
    toWad,
    wad4human,
    toBN,
} = require("@decentral.ee/web3-helpers");

contract("Scenario: Bob flows to Alice", (accounts) => {
    const [, alice, bob] = accounts;
    let sf;

    function sleep(ms) {
        return new Promise((resolve) => setTimeout(resolve, ms));
    }

    before(async () => {
        sf = new SuperfluidSDK.Framework({
            isTruffle: true,
            version: process.env.RELEASE_VERSION || "test",
            tokens: ["fDAI"],
        });
        await sf.initialize();
        if (toBN(await sf.tokens.fDAIx.balanceOf.call(bob)).lt(toWad(50))) {
            const fDAI = await sf.contracts.TestToken.at(
                sf.tokens.fDAI.address
            );
            await web3tx(fDAI.mint, "Mint 100 fDAI to bob")(bob, toWad(100));
            await web3tx(fDAI.approve, "Bob approves 100 fDAI to fDAIx")(
                sf.tokens.fDAIx.address,
                toWad(100),
                { from: bob }
            );
            await web3tx(sf.tokens.fDAIx.upgrade, "Bob upgrades 100 fDAIx")(
                toWad(100),
                { from: bob }
            );
        }
    });

    it("Bob creates a flow to Alice", async () => {
        const currentFlow = sf.cfa.getFlow({
            superToken: sf.tokens.fDAIx.address,
            sender: bob,
            receiver: alice,
        });
        if (currentFlow.flowRate !== "0") {
            await web3tx(
                sf.cfa.createFlow,
                "createFlow"
            )({
                superToken: sf.tokens.fDAIx.address,
                sender: bob,
                receiver: alice,
                flowRate: toWad(1000)
                    .divn(30 * 24 * 3600)
                    .toString(), // 10/30d
            });
        }
    });

    it("Observing flows between Bob and Alice", async () => {
        for (let i = 0; i < 5; ++i) {
            await sleep(5000);
            const b = await sf.tokens.fDAIx.balanceOf(bob);
            console.log("Bob balance", new Date(), wad4human(b));
        }
    });

    it("Bob deletes the flow", async () => {
        await web3tx(
            sf.cfa.deleteFlow,
            "deleteFlow"
        )({
            superToken: sf.tokens.fDAIx.address,
            sender: bob,
            receiver: alice,
        });
    });
});
