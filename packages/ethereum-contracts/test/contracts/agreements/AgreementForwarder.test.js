const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const AgreementForwarder = artifacts.require("AgreementForwarder");
const {toWad, toBN} = require("@decentral.ee/web3-helpers");

const mintAmount = "1000000000000000000000000000"; // a small loan of a billion dollars
const flowRate = "1000000000000";
const flowRate2 = "2000000000000";
const indexId = 1;
const distributionAmount = toWad("42");

describe("Agreement Forwarder", function () {
    const t = TestEnvironment.getSingleton();
    const {ZERO_ADDRESS} = t.constants;
    let superToken, host, cfa, ida, governance, agrFwd;
    let alice, bob, carol;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        cfa = t.contracts.cfa;
        ida = t.contracts.ida;
        host = t.contracts.superfluid;
        governance = t.contracts.governance;

        agrFwd = await AgreementForwarder.new(host.address);

        await governance.enableTrustedForwarder(
            host.address,
            ZERO_ADDRESS,
            agrFwd.address
        );

        ({alice, bob, carol} = t.aliases);
    });

    beforeEach(async () => {
        superToken = await SuperTokenMock.new(host.address, "69");
        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob, mintAmount, "0x", "0x");
    });

    describe("Constant Flow Ops", async function () {
        it("Revert on negative flowrate", async () => {
            await expectRevertedWith(
                agrFwd.setFlow(superToken.address, alice, flowRate, {
                    from: alice,
                }),
                "CFA: no self flow"
            );
        });

        it("Forward correct error", async () => {
            await expectRevertedWith(
                agrFwd.setFlow(superToken.address, alice, flowRate, {
                    from: alice,
                }),
                "CFA: no self flow"
            );
        });

        it("Set flow without pre-existing flow (create)", async () => {
            await agrFwd.setFlow(superToken.address, bob, flowRate, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate
            );
        });

        it("Set flow with pre-existing flow (update)", async () => {
            await agrFwd.setFlow(superToken.address, bob, flowRate, {
                from: alice,
            });

            await agrFwd.setFlow(superToken.address, bob, flowRate2, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate2
            );
        });

        it("Set flow to 0 with pre-existing flow (delete)", async () => {
            await agrFwd.setFlow(superToken.address, bob, flowRate, {
                from: alice,
            });

            await agrFwd.setFlow(superToken.address, bob, 0, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("Set flow to 0 without pre-existing flow (do nothing)", async () => {
            await agrFwd.setFlow(superToken.address, bob, 0, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("Set flow with same flowrate as before (do nothing)", async () => {
            await agrFwd.setFlow(superToken.address, bob, flowRate, {
                from: alice,
            });
            await agrFwd.setFlow(superToken.address, bob, flowRate, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowRate
            );
        });
    });

    describe("Instant Distribution Ops", async function () {
        it("Create an index", async () => {
            await agrFwd.createIndex(superToken.address, indexId, {
                from: alice,
            });

            assert.equal(
                (await ida.getIndex(superToken.address, alice, indexId)).exist,
                true
            );
        });

        it("Distribute to index", async () => {
            await agrFwd.createIndex(superToken.address, indexId, {
                from: alice,
            });
            await agrFwd.updateSubscription(
                superToken.address,
                indexId,
                bob,
                100,
                {
                    from: alice,
                }
            );
            await agrFwd.updateSubscription(
                superToken.address,
                indexId,
                carol,
                100,
                {
                    from: alice,
                }
            );

            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: bob,
                }
            );

            const bobBalBefore = await superToken.balanceOf(bob);

            await agrFwd.distribute(
                superToken.address,
                indexId,
                distributionAmount,
                {
                    from: alice,
                }
            );

            const bobBalAfter = await superToken.balanceOf(bob);

            assert.equal(
                bobBalAfter.toString(),
                bobBalBefore.add(distributionAmount.div(toBN(2))).toString()
            );
        });

        it("Use batch subscription update", async () => {
            await agrFwd.createIndex(superToken.address, indexId, {
                from: alice,
            });
            await agrFwd.updateSubscriptions(
                superToken.address,
                indexId,
                [bob, carol],
                [3, 4],
                {
                    from: alice,
                }
            );

            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: bob,
                }
            );
            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: carol,
                }
            );

            const bobBalBefore = await superToken.balanceOf(bob);
            const carolBalBefore = await superToken.balanceOf(carol);

            await agrFwd.distribute(
                superToken.address,
                indexId,
                distributionAmount,
                {
                    from: alice,
                }
            );

            const bobBalAfter = await superToken.balanceOf(bob);
            const carolBalAfter = await superToken.balanceOf(carol);

            assert.equal(
                bobBalAfter.toString(),
                bobBalBefore
                    .add(distributionAmount.mul(toBN(3)).div(toBN(7)))
                    .toString()
            );
            assert.equal(
                carolBalAfter.toString(),
                carolBalBefore
                    .add(distributionAmount.mul(toBN(4)).div(toBN(7)))
                    .toString()
            );
        });

        it("Use batch subscription update with some subscriptions pre-existing", async () => {
            await agrFwd.createIndex(superToken.address, indexId, {
                from: alice,
            });
            await agrFwd.updateSubscription(
                superToken.address,
                indexId,
                bob,
                1,
                {
                    from: alice,
                }
            );
            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: bob,
                }
            );
            await agrFwd.updateSubscriptions(
                superToken.address,
                indexId,
                [bob, carol],
                [5, 2],
                {
                    from: alice,
                }
            );

            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: carol,
                }
            );

            const bobBalBefore = await superToken.balanceOf(bob);
            const carolBalBefore = await superToken.balanceOf(carol);

            await agrFwd.distribute(
                superToken.address,
                indexId,
                distributionAmount,
                {
                    from: alice,
                }
            );

            const bobBalAfter = await superToken.balanceOf(bob);
            const carolBalAfter = await superToken.balanceOf(carol);

            assert.equal(
                bobBalAfter.toString(),
                bobBalBefore
                    .add(distributionAmount.mul(toBN(5)).div(toBN(7)))
                    .toString()
            );
            assert.equal(
                carolBalAfter.toString(),
                carolBalBefore
                    .add(distributionAmount.mul(toBN(2)).div(toBN(7)))
                    .toString()
            );
        });

        it("Create index with initial subscriptions", async () => {
            await agrFwd.createIndexWithSubscriptions(
                superToken.address,
                indexId,
                [bob, carol],
                [3, 4],
                {
                    from: alice,
                }
            );

            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: bob,
                }
            );
            await agrFwd.approveSubscription(
                superToken.address,
                alice,
                indexId,
                {
                    from: carol,
                }
            );

            const bobBalBefore = await superToken.balanceOf(bob);
            const carolBalBefore = await superToken.balanceOf(carol);

            await agrFwd.distribute(
                superToken.address,
                indexId,
                distributionAmount,
                {
                    from: alice,
                }
            );

            const bobBalAfter = await superToken.balanceOf(bob);
            const carolBalAfter = await superToken.balanceOf(carol);

            assert.equal(
                bobBalAfter.toString(),
                bobBalBefore
                    .add(distributionAmount.mul(toBN(3)).div(toBN(7)))
                    .toString()
            );
            assert.equal(
                carolBalAfter.toString(),
                carolBalBefore
                    .add(distributionAmount.mul(toBN(4)).div(toBN(7)))
                    .toString()
            );
        });
    });
});
