const { expectRevert } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-helpers");

const Tester = require("./Tester");

const DEFAULT_INDEX_ID = 42;

contract("Instance Distribution Agreement v1", accounts => {

    const tester = new Tester(accounts.slice(0, 5));
    const { alice, bob, carol, dan } = tester.aliases;
    const { INIT_BALANCE } = tester.constants;

    let superToken;
    let ida;
    let superfluid;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            superToken,
            ida,
            superfluid
        } = tester.contracts);
    });

    describe("#1 end to end", async () => {
        it("#1.1 1to3 distribution scenario", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            let pdata;
            let sunits;

            await web3tx(superfluid.callAgreement, "Alice create default index")(
                ida.address,
                ida.contract.methods.createIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, alice, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue, "0");
            assert.equal(pdata.totalUnits, "0");

            const subscribers = [
                ["Bob", bob, toWad("0.0001")],
                ["Carol", carol, toWad("0.0002")],
                ["Dan", dan, toWad("0.0003")],
            ];
            for (let i = 0; i < subscribers.length; ++i) {
                const subscriberName = subscribers[i][0];
                const subscriberAddr = subscribers[i][1];
                const subscriptionUnits = subscribers[i][2];
                await web3tx(superfluid.callAgreement, `${subscriberName} approves subscription to Alice`)(
                    ida.address,
                    ida.contract.methods.approveSubscription(
                        superToken.address,
                        alice,
                        DEFAULT_INDEX_ID,
                        "0x"
                    ).encodeABI(),
                    {
                        from: subscriberAddr,
                    }
                );
                sunits = await ida.getSubscriptionUnits.call(
                    superToken.address, alice, DEFAULT_INDEX_ID, subscriberAddr);
                assert.equal(sunits.toString(), "0");
                await web3tx(
                    superfluid.callAgreement,
                    `Alice updates ${subscriberName}'s subscription with ${wad4human(subscriptionUnits)} units`
                )(
                    ida.address,
                    ida.contract.methods.updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        subscriberAddr,
                        subscriptionUnits.toString(),
                        "0x"
                    ).encodeABI(),
                    {
                        from: alice,
                    }
                );
                sunits = await ida.getSubscriptionUnits.call(
                    superToken.address, alice, DEFAULT_INDEX_ID, subscriberAddr);
                assert.equal(sunits.toString(), subscriptionUnits.toString());

                const subs = await ida.listSubscriptions.call(superToken.address, subscriberAddr);
                assert.equal(subs.publishers.length, 1);
                assert.equal(subs.publishers[0], alice);
                assert.equal(subs.indexIds[0].toString(), DEFAULT_INDEX_ID.toString());
                assert.equal(subs.unitsList[0].toString(), subscriptionUnits.toString());
            }

            await web3tx(superfluid.callAgreement, "Alice distribute tokens")(
                ida.address,
                ida.contract.methods.updateIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    100,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, alice, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue.toString(), "100");
            assert.equal(pdata.totalUnits.toString(), toWad("0.0006").toString());

            assert.equal(
                (await superToken.balanceOf.call(bob)).toString(),
                toWad("0.01").toString());
            assert.equal(
                (await superToken.balanceOf.call(carol)).toString(),
                toWad("0.02").toString());
            assert.equal(
                (await superToken.balanceOf.call(dan)).toString(),
                toWad("0.03").toString());
            // assert.equal(
            //     (await superToken.balanceOf.call(alice)).toString(),
            //     toWad("100").toString());

            await web3tx(superfluid.callAgreement, "Alice distribute tokens again")(
                ida.address,
                ida.contract.methods.updateIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    300,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, alice, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue.toString(), "300");
            assert.equal(pdata.totalUnits.toString(), toWad("0.0006").toString());
            assert.equal(
                (await superToken.balanceOf.call(bob)).toString(),
                toWad("0.03").toString());
            assert.equal(
                (await superToken.balanceOf.call(carol)).toString(),
                toWad("0.06").toString());
            assert.equal(
                (await superToken.balanceOf.call(dan)).toString(),
                toWad("0.09").toString());
            // assert.equal(
            //     (await superToken.balanceOf.call(alice)).toString(),
            //     toWad("100").toString());

        });
    });

    describe("#2 createIndex", async () => {
        it("#2.1 should create a new index", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});

            await web3tx(superfluid.callAgreement, "Alice create default index")(
                ida.address,
                ida.contract.methods.createIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
        });

        it("#2.1 should fail to create the same index", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});

            await web3tx(superfluid.callAgreement, "Alice create default index")(
                ida.address,
                ida.contract.methods.createIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );

            await expectRevert(web3tx(superfluid.callAgreement, "Alice create the same index")(
                ida.address,
                ida.contract.methods.createIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            ), "IDAv1: index already exists");
        });
    });

});
