const { web3tx, toWad } = require("@decentral.ee/web3-helpers");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");
const SuperfluidSDK = require("@superfluid-finance/js-sdk");
const DividendRightsToken = artifacts.require("DividendRightsToken");

contract("DividendRightsToken", accounts => {
    const errorHandler = err => {
        if (err) throw err;
    };

    let sf;
    let dai;
    let daix;
    let app;

    const INIT_BALANCE = toWad(100);
    const MAX_UINT256 =
        "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    accounts = accounts.slice(0, 4);
    const [admin, alice, bob, carol] = accounts;

    before(async function() {
        await deployFramework(errorHandler, {
            web3,
            from: admin
        });
    });

    beforeEach(async function() {
        await deployTestToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });
        await deploySuperToken(errorHandler, [":", "fDAI"], {
            web3,
            from: admin
        });

        sf = new SuperfluidSDK.Framework({
            web3,
            version: "test",
            tokens: ["fDAI"]
        });
        await sf.initialize();

        if (!dai) {
            const daiAddress = await sf.tokens.fDAI.address;
            dai = await sf.contracts.TestToken.at(daiAddress);
            for (let i = 0; i < accounts.length; ++i) {
                await web3tx(dai.mint, `Account ${i} mints many dai`)(
                    accounts[i],
                    toWad(10000000),
                    { from: accounts[i] }
                );
            }
        }

        daix = sf.tokens.fDAIx;

        app = await web3tx(
            DividendRightsToken.new,
            "DividendRightsToken.new by alice"
        )(
            "Dividend Rights Token",
            "DRT",
            daix.address,
            sf.host.address,
            sf.agreements.ida.address,
            { from: alice }
        );
    });

    it("#1 end to end scenario", async () => {
        await dai.approve(daix.address, INIT_BALANCE, { from: alice });
        await daix.upgrade(INIT_BALANCE, { from: alice });

        // setup the app
        await web3tx(daix.approve, "Alice approve the app")(
            app.address,
            MAX_UINT256,
            { from: alice }
        );

        // alice issue rights to bob then got approved
        await web3tx(app.issue, "Alice issue 100 rights to bob")(bob, "100", {
            from: alice
        });
        assert.equal((await app.balanceOf.call(bob)).toString(), "100");
        assert.isFalse(await app.isSubscribing.call(bob));
        await web3tx(
            sf.host.callAgreement,
            "Bob approves subscription to the app"
        )(
            sf.agreements.ida.address,
            sf.agreements.ida.contract.methods
                .approveSubscription(daix.address, app.address, 0, "0x")
                .encodeABI(),
            "0x", // user data
            {
                from: bob
            }
        );
        assert.isTrue(await app.isSubscribing.call(bob));

        // alice issue rights to carol after approval
        assert.isFalse(await app.isSubscribing.call(carol));
        await web3tx(
            sf.host.callAgreement,
            "Carol approves subscription to the app"
        )(
            sf.agreements.ida.address,
            sf.agreements.ida.contract.methods
                .approveSubscription(daix.address, app.address, 0, "0x")
                .encodeABI(),
            "0x", // user data
            {
                from: carol
            }
        );
        assert.isTrue(await app.isSubscribing.call(carol));
        await web3tx(app.issue, "Alice issue 200 rights to carol")(
            carol,
            "200",
            { from: alice }
        );
        assert.equal((await app.balanceOf.call(carol)).toString(), "200");

        // console.log("!!!!",
        //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsApproved.toString(),
        //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsPending.toString(),
        //     (await daix.balanceOf.call(alice)).toString(),
        //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, bob)).units.toString(),
        //     (await daix.balanceOf.call(bob)).toString(),
        //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, carol)).units.toString(),
        //     (await daix.balanceOf.call(carol)).toString());

        // alice distribute 3 tokens
        await web3tx(
            app.distribute,
            "Alice distribute 3 tokens to everyone"
        )(toWad("3"), { from: alice });
        assert.equal(
            (await daix.balanceOf.call(alice)).toString(),
            toWad("97").toString()
        );
        assert.equal(
            (await daix.balanceOf.call(bob)).toString(),
            toWad("1").toString()
        );
        assert.equal(
            (await daix.balanceOf.call(carol)).toString(),
            toWad("2").toString()
        );

        // carol transfer 100 tokens to bob
        await web3tx(app.transfer, "Carol transfers 100 rights to bob")(
            bob,
            "100",
            { from: carol }
        );
        assert.equal((await app.balanceOf.call(bob)).toString(), "200");
        assert.equal((await app.balanceOf.call(carol)).toString(), "100");

        // console.log("!!!!",
        //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsApproved.toString(),
        //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsPending.toString(),
        //     (await daix.balanceOf.call(alice)).toString(),
        //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, bob)).units.toString(),
        //     (await daix.balanceOf.call(bob)).toString(),
        //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, carol)).units.toString(),
        //     (await daix.balanceOf.call(carol)).toString());

        // alice distribute 3 tokens
        await web3tx(
            app.distribute,
            "Alice distribute 3 tokens to everyone again"
        )(toWad("3"), { from: alice });
        assert.equal(
            (await daix.balanceOf.call(alice)).toString(),
            toWad("94").toString()
        );
        assert.equal(
            (await daix.balanceOf.call(bob)).toString(),
            toWad("3").toString()
        );
        assert.equal(
            (await daix.balanceOf.call(carol)).toString(),
            toWad("3").toString()
        );
    });
});
