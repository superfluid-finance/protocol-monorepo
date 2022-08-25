const TestEnvironment = require("../../TestEnvironment");
const {expectRevertedWith} = require("../../utils/expectRevert");
const {expectEvent} = require("@openzeppelin/test-helpers");
const SuperTokenMock = artifacts.require("SuperTokenMock");
const CFAv1Forwarder = artifacts.require("CFAv1Forwarder");

const mintAmount = "1000000000000000000000000000"; // a small loan of a billion dollars
const flowrate = "1000000000000";
const flowrate2 = "2000000000000";

describe("Agreement Forwarder", function () {
    const t = TestEnvironment.getSingleton();
    const {ZERO_ADDRESS} = t.constants;
    let superToken, host, cfa, governance, cfaFwd;
    let alice, bob, carol;

    // ACL flags
    const ALLOW_CREATE = 1 << 0;
    const ALLOW_UPDATE = 1 << 1;
    const ALLOW_DELETE = 1 << 2;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;
        governance = t.contracts.governance;

        cfaFwd = await CFAv1Forwarder.new(host.address);

        await governance.enableTrustedForwarder(
            host.address,
            ZERO_ADDRESS,
            cfaFwd.address
        );

        ({alice, bob, carol} = t.aliases);
    });

    beforeEach(async () => {
        superToken = await SuperTokenMock.new(host.address, "69");
        await superToken.mintInternal(alice, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob, mintAmount, "0x", "0x");
    });

    describe("Convenience methods", async function () {
        it("Revert on negative flowrate", async () => {
            await expectRevertedWith(
                cfaFwd.setFlowrate(superToken.address, alice, flowrate, {
                    from: alice,
                }),
                "CFA: no self flow"
            );
        });

        it("On failure: provide correct error", async () => {
            await expectRevertedWith(
                cfaFwd.setFlowrate(superToken.address, alice, flowrate, {
                    from: alice,
                }),
                "CFA: no self flow"
            );
        });

        it("Set flow without pre-existing flow (create)", async () => {
            await cfaFwd.setFlowrate(superToken.address, bob, flowrate, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );
        });

        it("Set flow with pre-existing flow (update)", async () => {
            await cfaFwd.setFlowrate(superToken.address, bob, flowrate, {
                from: alice,
            });

            await cfaFwd.setFlowrate(superToken.address, bob, flowrate2, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );
        });

        it("Set flow to 0 with pre-existing flow (delete)", async () => {
            await cfaFwd.setFlowrate(superToken.address, bob, flowrate, {
                from: alice,
            });

            await cfaFwd.setFlowrate(superToken.address, bob, 0, {
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
            await cfaFwd.setFlowrate(superToken.address, bob, 0, {
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
            await cfaFwd.setFlowrate(superToken.address, bob, flowrate, {
                from: alice,
            });
            await cfaFwd.setFlowrate(superToken.address, bob, flowrate, {
                from: alice,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );
        });

        it("Grant all flowOperator permissions", async () => {
            await cfaFwd.grantPermissions(superToken.address, carol, {
                from: alice,
            });

            // bob is not authorized
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(
                    superToken.address,
                    alice,
                    bob,
                    flowrate,
                    {
                        from: bob,
                    }
                ),
                "CFA: E_NO_OPERATOR_CREATE_FLOW"
            );

            // create
            await cfaFwd.setFlowrateFrom(
                superToken.address,
                alice,
                bob,
                flowrate,
                {
                    from: carol,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            // update
            await cfaFwd.setFlowrateFrom(
                superToken.address,
                alice,
                bob,
                flowrate2,
                {
                    from: carol,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            // delete
            await cfaFwd.setFlowrateFrom(superToken.address, alice, bob, 0, {
                from: carol,
            });
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                0
            );
        });

        it("Revoke all flowOperator permissions", async () => {
            await cfaFwd.grantPermissions(superToken.address, carol, {
                from: alice,
            });

            // create
            await cfaFwd.setFlowrateFrom(
                superToken.address,
                alice,
                bob,
                flowrate,
                {
                    from: carol,
                }
            );

            await cfaFwd.revokePermissions(superToken.address, carol, {
                from: alice,
            });

            // update denied
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(
                    superToken.address,
                    alice,
                    bob,
                    flowrate2,
                    {
                        from: carol,
                    }
                ),
                "E_NO_OPERATOR_UPDATE_FLOW"
            );

            // delete denied
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(superToken.address, alice, bob, 0, {
                    from: carol,
                }),
                "E_NO_OPERATOR_DELETE_FLOW"
            );
        });
    });

    describe("Wrapper methods", async function () {
        it("updateFlowOperatorPermissions: enforce flowrateAllowance", async () => {
            await cfaFwd.updateFlowOperatorPermissions(
                superToken.address,
                carol,
                ALLOW_CREATE | ALLOW_UPDATE,
                flowrate,
                {
                    from: alice,
                }
            );

            // create with excess flowrate
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(
                    superToken.address,
                    alice,
                    bob,
                    flowrate2,
                    {
                        from: carol,
                    }
                ),
                "CFA: E_EXCEED_FLOW_RATE_ALLOWANCE"
            );

            // create with allowed flowrate
            await cfaFwd.setFlowrateFrom(
                superToken.address,
                alice,
                bob,
                Number(flowrate) - 1,
                {
                    from: carol,
                }
            );

            // update with excess flowrate
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(
                    superToken.address,
                    alice,
                    bob,
                    flowrate2,
                    {
                        from: carol,
                    }
                ),
                "CFA: E_EXCEED_FLOW_RATE_ALLOWANCE"
            );

            // update with allowed flowrate
            await cfaFwd.setFlowrateFrom(
                superToken.address,
                alice,
                bob,
                Number(flowrate) - 1,
                {
                    from: carol,
                }
            );

            // permission for close not granted
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(superToken.address, alice, bob, 0, {
                    from: carol,
                }),
                "E_NO_OPERATOR_DELETE_FLOW"
            );
        });

        it("updateFlowOperatorPermissions: can only delete", async () => {
            // alice starts flow to bob...
            cfaFwd.setFlowrate(superToken.address, bob, flowrate2, {
                from: alice,
            }),
                // and gives carol permission to delete flows
                await cfaFwd.updateFlowOperatorPermissions(
                    superToken.address,
                    carol,
                    ALLOW_DELETE,
                    flowrate,
                    {
                        from: alice,
                    }
                );

            // carol: no update permission
            await expectRevertedWith(
                cfaFwd.setFlowrateFrom(
                    superToken.address,
                    alice,
                    bob,
                    Number(flowrate) - 1,
                    {
                        from: carol,
                    }
                ),
                "E_NO_OPERATOR_UPDATE_FLOW"
            );

            // but can delete
            await cfaFwd.setFlowrateFrom(superToken.address, alice, bob, 0, {
                from: carol,
            });

            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                0
            );
        });

        it("create/update/delete Flow", async () => {
            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: alice,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            await cfaFwd.updateFlow(
                superToken.address,
                alice,
                bob,
                flowrate2,
                "0x",
                {
                    from: alice,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            await cfaFwd.deleteFlow(superToken.address, alice, bob, "0x", {
                from: alice,
            });
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                0
            );
        });

        it("create/update/delete Flow by flowOperator", async () => {
            await cfaFwd.grantPermissions(superToken.address, carol, {
                from: alice,
            });

            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: carol,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            await cfaFwd.updateFlow(
                superToken.address,
                alice,
                bob,
                flowrate2,
                "0x",
                {
                    from: carol,
                }
            );
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            await cfaFwd.deleteFlow(superToken.address, alice, bob, "0x", {
                from: carol,
            });
            assert.equal(
                (
                    await cfa.getFlow(superToken.address, alice, bob)
                ).flowRate.toString(),
                0
            );
        });

        it("create/update/delete Flow with userData", async () => {
            const r1 = await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0xd1",
                {
                    from: alice,
                }
            );
            await expectEvent.inTransaction(
                r1.tx,
                t.contracts.cfa,
                "FlowUpdated",
                {
                    userData: "0xd1",
                }
            );

            const r2 = await cfaFwd.updateFlow(
                superToken.address,
                alice,
                bob,
                flowrate2,
                "0xd2",
                {
                    from: alice,
                }
            );
            await expectEvent.inTransaction(
                r2.tx,
                t.contracts.cfa,
                "FlowUpdated",
                {
                    userData: "0xd2",
                }
            );

            const r3 = await cfaFwd.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0xd3",
                {
                    from: alice,
                }
            );
            await expectEvent.inTransaction(
                r3.tx,
                t.contracts.cfa,
                "FlowUpdated",
                {
                    userData: "0xd3",
                }
            );
        });
    });

    describe("Readonly methods", async function () {
        it("getFlowrate", async () => {
            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: alice,
                }
            );
            assert.equal(
                (
                    await cfaFwd.getFlowrate(superToken.address, alice, bob)
                ).toString(),
                flowrate
            );
        });

        it("getFlowInfo", async () => {
            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: alice,
                }
            );

            const flowInfo = await cfaFwd.getFlowInfo(
                superToken.address,
                alice,
                bob
            );
            const flow = await cfa.getFlow(superToken.address, alice, bob);

            assert.equal(
                flowInfo.lastUpdated.toString(),
                flow.timestamp.toString()
            );
            assert.equal(
                flowInfo.flowrate.toString(),
                flow.flowRate.toString()
            );
            assert.equal(flowInfo.deposit.toString(), flow.deposit.toString());
            assert.equal(
                flowInfo.owedDeposit.toString(),
                flow.owedDeposit.toString()
            );
        });

        it("getBufferAmountByFlowrate", async () => {
            const ba = await cfa.getDepositRequiredForFlowRate(
                superToken.address,
                flowrate
            );
            const baW = await cfaFwd.getBufferAmountByFlowrate(
                superToken.address,
                flowrate
            );
            assert.equal(ba.toString(), baW.toString());
        });

        it("getAccountFlowrate", async () => {
            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: alice,
                }
            );
            await cfaFwd.createFlow(
                superToken.address,
                bob,
                alice,
                Number(flowrate) - 1,
                "0x",
                {
                    from: bob,
                }
            );

            const accFrAlice = await cfaFwd.getAccountFlowrate(
                superToken.address,
                alice
            );
            const accFrBob = await cfaFwd.getAccountFlowrate(
                superToken.address,
                bob
            );

            assert.equal(accFrAlice.toString(), "-1");
            assert.equal(accFrBob.toString(), "1");
        });

        it("getAccountFlowInfo", async () => {
            await cfaFwd.createFlow(
                superToken.address,
                alice,
                bob,
                flowrate,
                "0x",
                {
                    from: alice,
                }
            );
            await cfaFwd.createFlow(
                superToken.address,
                bob,
                alice,
                Number(flowrate) - 1,
                "0x",
                {
                    from: bob,
                }
            );

            const accFIAliceI = await cfa.getAccountFlowInfo(
                superToken.address,
                alice
            );
            const accFIAliceW = await cfaFwd.getAccountFlowInfo(
                superToken.address,
                alice
            );
            const accFIBobI = await cfa.getAccountFlowInfo(
                superToken.address,
                bob
            );
            const accFIBobW = await cfaFwd.getAccountFlowInfo(
                superToken.address,
                bob
            );

            assert.equal(
                accFIAliceW.lastUpdated.toString(),
                accFIAliceI.timestamp.toString()
            );
            assert.equal(
                accFIAliceW.flowrate.toString(),
                accFIAliceI.flowRate.toString()
            );
            assert.equal(
                accFIAliceW.deposit.toString(),
                accFIAliceI.deposit.toString()
            );
            assert.equal(
                accFIAliceW.owedDeposit.toString(),
                accFIAliceI.owedDeposit.toString()
            );

            assert.equal(
                accFIBobW.lastUpdated.toString(),
                accFIBobI.timestamp.toString()
            );
            assert.equal(
                accFIBobW.flowrate.toString(),
                accFIBobI.flowRate.toString()
            );
            assert.equal(
                accFIBobW.deposit.toString(),
                accFIBobI.deposit.toString()
            );
            assert.equal(
                accFIBobW.owedDeposit.toString(),
                accFIBobI.owedDeposit.toString()
            );
        });

        it("getFlowOperatorPermissions", async () => {
            const permPre = await cfaFwd.getFlowOperatorPermissions(
                superToken.address,
                alice,
                carol
            );
            assert.equal(permPre.permissions, 0);
            assert.equal(permPre.flowrateAllowance, 0);

            await cfaFwd.updateFlowOperatorPermissions(
                superToken.address,
                carol,
                ALLOW_DELETE,
                flowrate,
                {
                    from: alice,
                }
            );

            const permPost = await cfaFwd.getFlowOperatorPermissions(
                superToken.address,
                alice,
                carol
            );
            assert.equal(permPost.permissions, ALLOW_DELETE);
            assert.equal(permPost.flowrateAllowance, flowrate);
        });
    });
});
