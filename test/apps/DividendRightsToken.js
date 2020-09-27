const Tester = require("../superfluid/Tester");
const DividendRightsToken = artifacts.require("DividendRightsToken");

const {
    web3tx,
    toWad,
} = require("@decentral.ee/web3-helpers");

contract("DividendRightsToken", accounts => {

    const tester = new Tester(accounts);

    let superToken;
    let ida;
    let superfluid;

    const { MAX_UINT256, INIT_BALANCE } = tester.constants;
    const { alice, bob, carol } = tester.aliases;

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            superToken,
            ida,
            superfluid
        } = tester.contracts);
    });

    it("#1 end to end scenario", async () => {
        await superToken.upgrade(INIT_BALANCE, { from: alice });

        // setup the app
        const app = await web3tx(DividendRightsToken.new, "DividendRightsToken.new by alice")(
            "Dividend Rights Token", "DRT",
            superToken.address,
            superfluid.address, ida.address,
            { from: alice }
        );
        await web3tx(superToken.approve, "Alice approve the app")(
            app.address, MAX_UINT256,
            { from: alice }
        );

        // alice issue rights to bob then got approved
        await web3tx(app.issue, "Alice issue 100 rights to bob")(
            bob, "100",
            { from: alice }
        );
        assert.equal(
            (await app.balanceOf.call(bob)).toString(),
            "100"
        );
        assert.isFalse(await app.isSubscribing.call(bob));
        await web3tx(superfluid.callAgreement, "Bob approves subscription to the app")(
            ida.address,
            ida.contract.methods.approveSubscription(
                superToken.address,
                app.address,
                0,
                "0x"
            ).encodeABI(),
            {
                from: bob,
            }
        );
        assert.isTrue(await app.isSubscribing.call(bob));

        // alice issue rights to carol after approval
        assert.isFalse(await app.isSubscribing.call(carol));
        await web3tx(superfluid.callAgreement, "Carol approves subscription to the app")(
            ida.address,
            ida.contract.methods.approveSubscription(
                superToken.address,
                app.address,
                0,
                "0x"
            ).encodeABI(),
            {
                from: carol,
            }
        );
        assert.isTrue(await app.isSubscribing.call(carol));
        await web3tx(app.issue, "Alice issue 200 rights to carol")(
            carol, "200",
            { from: alice }
        );
        assert.equal(
            (await app.balanceOf.call(carol)).toString(),
            "200"
        );

        // console.log("!!!!",
        //     (await ida.getIndex.call(superToken.address, app.address, 0)).totalUnitsApproved.toString(),
        //     (await ida.getIndex.call(superToken.address, app.address, 0)).totalUnitsPending.toString(),
        //     (await superToken.balanceOf.call(alice)).toString(),
        //     (await ida.getSubscription.call(superToken.address, app.address, 0, bob)).units.toString(),
        //     (await superToken.balanceOf.call(bob)).toString(),
        //     (await ida.getSubscription.call(superToken.address, app.address, 0, carol)).units.toString(),
        //     (await superToken.balanceOf.call(carol)).toString());

        // alice distribute 3 tokens
        await web3tx(app.distribute, "Alice distribute 3 tokens to everyone")(
            toWad("3"),
            { from: alice }
        );
        assert.equal(
            (await superToken.balanceOf.call(alice)).toString(),
            toWad("97").toString()
        );
        assert.equal(
            (await superToken.balanceOf.call(bob)).toString(),
            toWad("1").toString()
        );
        assert.equal(
            (await superToken.balanceOf.call(carol)).toString(),
            toWad("2").toString()
        );

        // carol transfer 100 tokens to bob
        await web3tx(app.transfer, "Carol transfers 100 rights to bob")(
            bob, "100",
            { from: carol }
        );
        assert.equal(
            (await app.balanceOf.call(bob)).toString(),
            "200"
        );
        assert.equal(
            (await app.balanceOf.call(carol)).toString(),
            "100"
        );

        // console.log("!!!!",
        //     (await ida.getIndex.call(superToken.address, app.address, 0)).totalUnitsApproved.toString(),
        //     (await ida.getIndex.call(superToken.address, app.address, 0)).totalUnitsPending.toString(),
        //     (await superToken.balanceOf.call(alice)).toString(),
        //     (await ida.getSubscription.call(superToken.address, app.address, 0, bob)).units.toString(),
        //     (await superToken.balanceOf.call(bob)).toString(),
        //     (await ida.getSubscription.call(superToken.address, app.address, 0, carol)).units.toString(),
        //     (await superToken.balanceOf.call(carol)).toString());

        // alice distribute 3 tokens
        await web3tx(app.distribute, "Alice distribute 3 tokens to everyone again")(
            toWad("3"),
            { from: alice }
        );
        assert.equal(
            (await superToken.balanceOf.call(alice)).toString(),
            toWad("94").toString()
        );
        assert.equal(
            (await superToken.balanceOf.call(bob)).toString(),
            toWad("3").toString()
        );
        assert.equal(
            (await superToken.balanceOf.call(carol)).toString(),
            toWad("3").toString()
        );
    });

});
