const {
    web3tx,
    toWad,
    toBN,
} = require("@decentral.ee/web3-helpers");
const { expectRevert } = require("@openzeppelin/test-helpers");
const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/ethereum-contracts");
const LotterySuperApp = artifacts.require("LotterySuperApp");


contract("LotterySuperApp", accounts => {

    const errorHandler = err => { if (err) throw err; };

    const ZERO_ADDRESS = "0x"+"0".repeat(40);
    const MINIMUM_GAME_FLOW_RATE = toWad(10).div(toBN(3600*24*30));

    accounts = accounts.slice(0, 4);
    const [admin, bob, carol, dan] = accounts;

    let sf;
    let dai;
    let daix;
    let app;

    beforeEach(async function () {
        await deployFramework(errorHandler);

        sf = new SuperfluidSDK.Framework({ web3Provider: web3.currentProvider });
        await sf.initialize();

        if (!dai) {
            await deployTestToken(errorHandler, [":", "fDAI"]);
            const daiAddress = await sf.resolver.get("tokens.fDAI");
            dai = await sf.contracts.TestToken.at(daiAddress);
            for (let i = 0; i < accounts.length; ++i) {
                await web3tx(dai.mint, `Account ${i} mints many dai`)(
                    accounts[i],
                    toWad(10000000),
                    { from: accounts[i] }
                );
            }
        }

        await deploySuperToken(errorHandler, [":", "fDAI"]);

        const daixWrapper = await sf.getERC20Wrapper(dai);
        daix = await sf.contracts.ISuperToken.at(daixWrapper.wrapperAddress);

        app = await web3tx(LotterySuperApp.new, "Deploy LotterySuperApp")(
            sf.host.address,
            sf.agreements.cfa.address,
            daix.address
        );

        for (let i = 0; i < accounts.length; ++i) {
            await web3tx(dai.approve, `Account ${i} approves daix`)(daix.address, toWad(100), { from: accounts[i] });
        }
    });   

    async function printRealtimeBalance(label, account) {
        const b = await daix.realtimeBalanceOfNow.call(account);
        console.log(`${label} realtime balance`,
            b.availableBalance.toString(),
            b.deposit.toString(),
            b.owedDeposit.toString());
        return b;
    }

    function createPlayBatchCall(upgradeAmount = 0) {
        return [
            [
                2, // upgrade 100 daix to play the game
                daix.address,
                web3.eth.abi.encodeParameters(
                    ["uint256"],
                    [toWad(upgradeAmount).toString()])
            ],
            [
                0, // approve the ticket fee
                daix.address,
                web3.eth.abi.encodeParameters(
                    ["address", "uint256"],
                    [app.address, toWad("1").toString()])
            ],
            [
                5, // callAppAction to participate
                app.address,
                app.contract.methods.participate("0x").encodeABI()
            ],
            [
                4, // create constant flow (10/mo)
                sf.agreements.cfa.address,
                sf.agreements.cfa.contract.methods.createFlow(
                    daix.address,
                    app.address,
                    MINIMUM_GAME_FLOW_RATE.toString(),
                    "0x"
                ).encodeABI()
            ]               
        ];
    }

    it("Lonely game case", async () => {
        let appRealtimeBalance;
        assert.equal((await app.currentWinner.call()).player, ZERO_ADDRESS);
        // bob is the first player
        await web3tx(sf.host.batchCall, "Bob joining the game")(
            createPlayBatchCall(100),
            { from: bob }
        );
        await expectRevert(sf.host.batchCall(
            createPlayBatchCall(0),
            { from: bob }
        ), "Flow already exist");
        assert.equal((await app.currentWinner.call()).player, bob);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        // bob quits the game
        await web3tx(sf.host.callAgreement, "Bob quiting the game")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                bob,
                app.address,
                "0x"
            ).encodeABI(),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, ZERO_ADDRESS);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        // bob is the only player again
        await web3tx(sf.host.batchCall, "Bob joining the game again")(
            createPlayBatchCall(),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, bob);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
    })

    it("Happy game case", async () => {
        let appRealtimeBalance;

        assert.equal((await app.currentWinner.call()).player, ZERO_ADDRESS);
        //
        // Round 1: +bob, +carol, -bob, - carol
        //
        await web3tx(sf.host.batchCall, "Bob joining the game")(
            createPlayBatchCall(100),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, bob);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        // carol enters the game
        await web3tx(sf.host.batchCall, "Carol joining the game too")(
            createPlayBatchCall(100),
            { from: carol }
        );
        let winner = (await app.currentWinner.call()).player;
        console.log("Winner", winner);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, winner)).toString(),
            MINIMUM_GAME_FLOW_RATE.toString());
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        // bob quits the game
        await web3tx(sf.host.callAgreement, "Bob quiting the game")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                bob,
                app.address,
                "0x"
            ).encodeABI(),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, carol);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, carol)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        // carol quits the game
        await web3tx(sf.host.callAgreement, "Carol quiting the game too")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                carol,
                app.address,
                "0x"
            ).encodeABI(),
            { from: carol }
        );
        assert.equal((await app.currentWinner.call()).player, ZERO_ADDRESS);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, carol)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        //
        // Round 2: +bob, +carol, -carol, -bob
        //
        // bob join the game again
        await web3tx(sf.host.batchCall, "Bob joining the game again")(
            createPlayBatchCall(),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, bob);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, carol)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        // carol join the game again too
        await web3tx(sf.host.batchCall, "Carol joining the game again too")(
            createPlayBatchCall(),
            { from: carol }
        );
        await web3tx(sf.host.callAgreement, "Carol quiting the game first this time")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                carol,
                app.address,
                "0x"
            ).encodeABI(),
            { from: carol }
        );
        assert.equal((await app.currentWinner.call()).player, bob);
        await web3tx(sf.host.callAgreement, "Bob quiting the game too")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                bob,
                app.address,
                "0x"
            ).encodeABI(),
            { from: bob }
        );
        assert.equal((await app.currentWinner.call()).player, ZERO_ADDRESS);
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, app.address)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, bob)).toString(),
            "0");
        assert.equal(
            (await sf.agreements.cfa.getNetFlow(daix.address, carol)).toString(),
            "0");
        appRealtimeBalance = await printRealtimeBalance("App", app.address);
        await printRealtimeBalance("Bob", bob);
        await printRealtimeBalance("Carol", carol);
        //
        // Round 3: +carol, +bob, -bob, - carol
        //
        await web3tx(sf.host.batchCall, "Carol joining the game first")(
            createPlayBatchCall(),
            { from: carol }
        );
        await web3tx(sf.host.batchCall, "Bob joining the game again")(
            createPlayBatchCall(),
            { from: bob }
        );
        await web3tx(sf.host.callAgreement, "Bob quiting the game")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                bob,
                app.address,
                "0x"
            ).encodeABI(),
            { from: bob }
        );
        await web3tx(sf.host.callAgreement, "Carol quiting the game")(
            sf.agreements.cfa.address,
            sf.agreements.cfa.contract.methods.deleteFlow(
                daix.address,
                carol,
                app.address,
                "0x"
            ).encodeABI(),
            { from: carol }
        );
    })

    it("Test randomness", async () => {
        const counters = {};
        counters[carol] = { name: "carol", count: 0 };
        counters[bob] = { name: "bob", count: 0 };
        counters[dan] = { name: "dan", count: 0 };
        await web3tx(sf.host.batchCall, "Carol joining the game")(
            createPlayBatchCall(100),
            { from: carol }
        );
        await web3tx(sf.host.batchCall, "Bob joining the game too")(
            createPlayBatchCall(100),
            { from: bob }
        );
        await web3tx(sf.host.batchCall, "Dan joining the game too")(
            createPlayBatchCall(100),
            { from: dan }
        );
        for (let i = 0; i < 20; ++i) {
            counters[(await app.currentWinner.call()).player].count++;
            await web3tx(sf.host.callAgreement, "Dan quiting the game")(
                sf.agreements.cfa.address,
                sf.agreements.cfa.contract.methods.deleteFlow(
                    daix.address,
                    dan,
                    app.address,
                    "0x"
                ).encodeABI(),
                { from: dan }
            );
            await web3tx(sf.host.batchCall, "Dan joining the game too")(
                createPlayBatchCall(),
                { from: dan }
            );
        }
        console.log("Winning counters", counters);
        assert.isTrue(counters[bob].count > 0);
        assert.isTrue(counters[carol].count > 0);
        assert.isTrue(counters[dan].count > 0);
    });
});

