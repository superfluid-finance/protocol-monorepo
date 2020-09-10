const { expectRevert } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-helpers");

const Tester = require("../superfluid/Tester");

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
                [bob, toWad("0.0001")],
                [carol, toWad("0.0002")],
                [dan, toWad("0.0003")],
            ];
            for (let i = 0; i < subscribers.length; ++i) {
                const subscriberAddr = subscribers[i][0];
                const subscriptionUnits = subscribers[i][1];
                const subscriberName = tester.toAliases[subscriberAddr];
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
            assert.equal(
                (await superToken.balanceOf.call(alice)).toString(),
                toWad("99.94").toString());

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

            const expectedBalances = [
                [bob,   toWad("0.03")],
                [carol, toWad("0.06")],
                [dan,   toWad("0.09")],
                [alice, toWad("99.82")],
            ];
            for (let i = 0; i < expectedBalances.length; ++i) {
                const account = expectedBalances[i][0];
                const expectedBalance = expectedBalances[i][1];
                const balance = await superToken.balanceOf.call(account);
                console.log(`${tester.toAliases[account]}'s current balance: `, wad4human(balance));
                assert.equal(balance.toString(), expectedBalance.toString());
            }

            await tester.validateSystem();
        });

        it("#1.2 2to1 distribution scenario", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: bob});
            let pdata;
            let sunits;
            let balance;

            // alice and bob create indeces and dan subscribes to them
            const publishers = [
                [alice, toWad("0.0001")],
                [bob, toWad("0.0002")],
            ];
            for (let i = 0; i < publishers.length; ++i) {
                let publisherAddr = publishers[i][0];
                let subscriptionUnits = publishers[i][1];
                const publisherName = tester.toAliases[publisherAddr];
                await web3tx(superfluid.callAgreement, `${publisherName} create default index`)(
                    ida.address,
                    ida.contract.methods.createIndex(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        "0x"
                    ).encodeABI(),
                    {
                        from: publisherAddr,
                    }
                );
                pdata = await ida.getIndex.call(superToken.address, publisherAddr, DEFAULT_INDEX_ID);
                assert.equal(pdata.indexValue, "0");
                assert.equal(pdata.totalUnits, "0");

                await web3tx(superfluid.callAgreement, `Dan approves subscription to ${publisherName}`)(
                    ida.address,
                    ida.contract.methods.approveSubscription(
                        superToken.address,
                        publisherAddr,
                        DEFAULT_INDEX_ID,
                        "0x"
                    ).encodeABI(),
                    {
                        from: dan,
                    }
                );
                await web3tx(
                    superfluid.callAgreement,
                    `${publisherName} updates Dan's subscription with ${wad4human(subscriptionUnits)} units`
                )(
                    ida.address,
                    ida.contract.methods.updateSubscription(
                        superToken.address,
                        DEFAULT_INDEX_ID,
                        dan,
                        subscriptionUnits.toString(),
                        "0x"
                    ).encodeABI(),
                    {
                        from: publisherAddr,
                    }
                );
                sunits = await ida.getSubscriptionUnits.call(
                    superToken.address, publisherAddr, DEFAULT_INDEX_ID, dan);
                assert.equal(sunits.toString(), subscriptionUnits.toString());
            }
            const subs = await ida.listSubscriptions.call(superToken.address, dan);
            assert.equal(subs.publishers.length, 2);
            assert.equal(subs.publishers[0], alice);
            assert.equal(subs.indexIds[0].toString(), DEFAULT_INDEX_ID.toString());
            assert.equal(wad4human(subs.unitsList[0]), "0.00010");
            assert.equal(subs.publishers[1], bob);
            assert.equal(subs.indexIds[1].toString(), DEFAULT_INDEX_ID.toString());
            assert.equal(wad4human(subs.unitsList[1]), "0.00020");

            balance = await superToken.balanceOf.call(dan);
            console.log("Dan's current balance: ", wad4human(balance));
            assert.equal(balance.toString(), "0");

            // Alice distributes tokens (100 * 0.0001 = 0.01)
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
            assert.equal(pdata.totalUnits.toString(), toWad("0.0001").toString());
            balance = await superToken.balanceOf.call(dan);
            console.log("Dan's current balance: ", wad4human(balance));
            assert.equal(balance.toString(), toWad("0.01").toString());

            // Bob distributes tokens (200 * 0.0002 = 0.04)
            await web3tx(superfluid.callAgreement, "Bob distribute tokens")(
                ida.address,
                ida.contract.methods.updateIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    200,
                    "0x"
                ).encodeABI(),
                {
                    from: bob,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, bob, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue.toString(), "200");
            assert.equal(pdata.totalUnits.toString(), toWad("0.0002").toString());
            balance = await superToken.balanceOf.call(dan);
            console.log("Dan's current balance: ", wad4human(balance));
            assert.equal(balance.toString(), toWad("0.05").toString());

            // Alice update Dan's subscription with more units
            await web3tx(
                superfluid.callAgreement,
                "Alice updates Dan's subscription with 0.0003 units"
            )(
                ida.address,
                ida.contract.methods.updateSubscription(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    dan,
                    toWad("0.0003").toString(),
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, alice, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue.toString(), "100");
            assert.equal(pdata.totalUnits.toString(), toWad("0.0003").toString());
            sunits = await ida.getSubscriptionUnits.call(
                superToken.address, alice, DEFAULT_INDEX_ID, dan);
            assert.equal(wad4human(sunits), "0.00030");
            balance = await superToken.balanceOf.call(dan);
            console.log("Dan's current balance: ", wad4human(balance));
            assert.equal(balance.toString(), toWad("0.05").toString());

            // Alice distributes tokens again (100 * 0.0003 = 0.03)
            await web3tx(superfluid.callAgreement, "Alice distribute tokens again")(
                ida.address,
                ida.contract.methods.updateIndex(
                    superToken.address,
                    DEFAULT_INDEX_ID,
                    200,
                    "0x"
                ).encodeABI(),
                {
                    from: alice,
                }
            );
            pdata = await ida.getIndex.call(superToken.address, alice, DEFAULT_INDEX_ID);
            assert.equal(pdata.indexValue.toString(), "200");
            assert.equal(pdata.totalUnits.toString(), toWad("0.0003").toString());
            balance = await superToken.balanceOf.call(dan);
            console.log("Dan's current balance: ", wad4human(balance));
            assert.equal(balance.toString(), toWad("0.08").toString());

            await tester.validateSystem();
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
