const { expectRevert } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");

const traveler = require("ganache-time-traveler");

const Tester = require("../superfluid/Tester");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);

contract("Constant Flow Agreement", accounts => {

    const tester = new Tester(accounts.slice(0, 5));
    const { admin, alice, bob, carol, dan } = tester.aliases;
    const { INIT_BALANCE } = tester.constants;

    let token;
    let superToken;
    let cfa;
    let superfluid;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            token,
            superToken,
            cfa,
            superfluid
        } = tester.contracts);
    });

    describe("#1 FlowAgreement.createFlow", () => {
        it("#1.1 should start a new flow", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            const dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            const aliceNetFlow = await cfa.getNetFlow.call(superToken.address, alice);
            const bobNetFlow = await cfa.getNetFlow.call(superToken.address, bob);

            assert.equal(aliceNetFlow.toString(), -FLOW_RATE, "Alice NetFlow is wrong");
            assert.equal(bobNetFlow.toString(), FLOW_RATE, "Bob NetFlow is wrong");

        });
    });

    describe("#1 FlowAgreement.updateFlow", () => {
        it("#1.1 should stream in correct flow rate with single flow", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            const deposit = toBN(tester.constants.LIQUIDATION_PERIOD * FLOW_RATE);

            const dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            // test flow views
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());

            assert.equal((await cfa.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());

            /*
            assert.equal((await cfa.getFlow.call(
                superToken.address, alice, bob
            ))[1].toString(), FLOW_RATE.toString());
            */

            assert.equal((await cfa.getFlow.call(
                superToken.address, bob, alice
            ))[1].toString(), "0");

            const beginBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            const endBlock = await web3.eth.getBlock("latest");

            const aliceBalance = await superToken.balanceOf.call(alice);
            const bobBalance = await superToken.balanceOf.call(bob);
            const span = endBlock.timestamp - beginBlock.timestamp;
            const aliceBalanceExpected = INIT_BALANCE - (span * FLOW_RATE);
            const bobBalanceExpected = (span * FLOW_RATE);
            const aliceDeposit = await superToken.realtimeBalanceOf.call(alice, endBlock.timestamp);

            assert.equal((aliceBalance.add(aliceDeposit.deposit)).toString(), aliceBalanceExpected.toString(),
                "Alice final balance is wrong");
            assert.equal(bobBalance.toString(), bobBalanceExpected.toString(),
                "Bob final balance is wrong");
            assert.equal(aliceDeposit.deposit.toString(), deposit.toString(), "Alice deposit is wrong");

            await tester.validateSystem();
        });

        it("#1.2 should stream in correct flow rate after two out flows of the same account", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            const deposit = toBN(tester.constants.LIQUIDATION_PERIOD * FLOW_RATE);

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx = await web3tx(superfluid.callAgreement, "CreateFlow alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            // test flow views
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());
            assert.equal((await cfa.getFlow.call(
                superToken.address, alice, bob
            ))[1].toString(), FLOW_RATE.toString());
            assert.equal((await cfa.getFlow.call(
                superToken.address, bob, alice
            ))[1].toString(), "0");

            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                carol,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx2 = await web3tx(superfluid.callAgreement, "CreateFlow alice -> carol")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
            await traveler.advanceTimeAndBlock(ADV_TIME);

            // test flow views
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-2)).toString());

            assert.equal((await cfa.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());

            assert.equal((await cfa.getNetFlow.call(
                superToken.address, carol
            )).toString(), FLOW_RATE.toString());

            assert.equal((await cfa.getFlow.call(
                superToken.address, alice, bob
            ))[1].toString(), FLOW_RATE.toString());

            assert.equal((await cfa.getFlow.call(
                superToken.address, bob, alice
            ))[1].toString(), "0");

            assert.equal((await cfa.getFlow.call(
                superToken.address, alice, carol
            ))[1].toString(), FLOW_RATE.toString());

            assert.equal((await cfa.getFlow.call(
                superToken.address, carol, alice
            ))[1].toString(), "0");

            assert.equal((await cfa.getFlow.call(
                superToken.address, bob,carol
            ))[1].toString(), "0");

            assert.equal((await cfa.getFlow.call(
                superToken.address, carol, bob
            ))[1].toString(), "0");

            const block1 = await web3.eth.getBlock(tx.receipt.blockNumber);
            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            const user1Balance = await superToken.balanceOf.call(alice);
            const user2Balance = await superToken.balanceOf.call(bob);
            const user3Balance = await superToken.balanceOf.call(carol);
            const endBlock = await web3.eth.getBlock("latest");

            const span2 = endBlock.timestamp - block1.timestamp;
            const span3 = endBlock.timestamp - block2.timestamp;

            const finalUser2 = (span2 * FLOW_RATE);
            const finalUser3 = (span3 * FLOW_RATE);
            const finalUser1 = INIT_BALANCE - finalUser2 - finalUser3;
            const aliceDeposit = await superToken.realtimeBalanceOf.call(alice, endBlock.timestamp);

            assert.equal(user1Balance.add(aliceDeposit.deposit).toString(),
                finalUser1.toString(), "User 1 Final balance is wrong");
            assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
            assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wring");
            assert.equal(aliceDeposit.deposit.toString(), (deposit * 2).toString(), "Alice deposit is wrong");

            await tester.validateSystem();
        });

        it("#1.3 update with negative flow rate should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                "-100000000000",
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ),"FlowAgreement: negative flow rate not allowed" );
        });

        it("#1.4 update active flow to negative flow rate should also fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();
            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
            await traveler.advanceTimeAndBlock(ADV_TIME);
            dataAgreement = cfa.contract.methods.updateFlow(
                superToken.address,
                bob,
                "-2000000000000000000",
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "FlowAgreement: negative flow rate not allowed");

            await tester.validateSystem();
        });

        /*
        it("Gas report - Constant Flow Agreement", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();
            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice creating flow to bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.updateFlow(
                superToken.address,
                bob,
                "1000000000000000000",
                "0x"
            ).encodeABI();
            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice updating flow to bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice deleting flow to bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
        });
    */

        it("#1.5 update other's flow should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.updateFlow(
                superToken.address,
                bob,
                "-2000000000000000000",
                "0x"
            ).encodeABI();

            await traveler.advanceTimeAndBlock(ADV_TIME);
            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob from carol account")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: admin,
                    }
                ), "Flow doesn't exist");

            await tester.validateSystem();
        });

        it("#1.6 update with zero rate should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
            await traveler.advanceTimeAndBlock(ADV_TIME);
            dataAgreement = cfa.contract.methods.updateFlow(
                superToken.address,
                bob,
                "0",
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob from carol account")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "FlowAgreement: use delete flow");

            await tester.validateSystem();
        });

        it("#1.7 should allow net flow rate 0 then back to normal rate", async() => {
            const deposit = toBN(tester.constants.LIQUIDATION_PERIOD * FLOW_RATE * 2);
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: carol});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx1 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                alice,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx2 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement carol -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: carol,
                }
            );
            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), "0");

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                dan,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();
            const tx3 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> dan")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            const block3 = await web3.eth.getBlock(tx3.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());

            const endBlock = await web3.eth.getBlock("latest");

            const span1 = toBN(block2.timestamp - block1.timestamp);
            const span2 = toBN(block3.timestamp - block2.timestamp);
            const span3 = toBN(endBlock.timestamp - block3.timestamp);

            const alice1Balance = await superToken.balanceOf.call(alice);
            const bobBalance = await superToken.balanceOf.call(bob);
            const carolBalance = await superToken.balanceOf.call(carol);
            const danBalance = await superToken.balanceOf.call(dan);

            const aliceDeposit = await superToken.realtimeBalanceOf(alice, endBlock.timestamp);
            const carolDeposit = await superToken.realtimeBalanceOf(carol, endBlock.timestamp);
            const aliceBalanceExpected = INIT_BALANCE
                .sub(span1.mul(FLOW_RATE))
                .sub(span3.mul(FLOW_RATE));
            const bobBalanceExpected = span1.add(span2).add(span3).mul(FLOW_RATE);
            const carolBalanceExpected = INIT_BALANCE
                .sub(span2.add(span3).mul(FLOW_RATE));
            const danBalanceExpected = span3.mul(FLOW_RATE);

            assert.equal(alice1Balance.add(aliceDeposit.deposit).toString(), aliceBalanceExpected.toString());
            assert.equal(bobBalance.toString(), bobBalanceExpected.toString());
            assert.equal(carolBalance.add(carolDeposit.deposit).toString(), carolBalanceExpected.toString());
            assert.equal(danBalance.toString(), danBalanceExpected.toString());
            assert.equal(aliceDeposit.deposit.toString(), deposit.toString());

            await tester.validateSystem();
        });

        it("#1.8 should update flow the second time to new flow rate", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx1 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);

            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());

            dataAgreement = cfa.contract.methods.updateFlow(
                superToken.address,
                bob,
                FLOW_RATE.mul(toBN(2)).toString(),
                "0x"
            ).encodeABI();

            const tx2 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);

            assert.equal((await cfa.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-2)).toString());

            const endBlock = await web3.eth.getBlock("latest");

            const span1 = toBN(block2.timestamp - block1.timestamp);
            const span2 = toBN(endBlock.timestamp - block2.timestamp);

            const alice1Balance = await superToken.balanceOf.call(alice);
            const aliceDeposit = await superToken.realtimeBalanceOf(alice, endBlock.timestamp);
            const bobBalance = await superToken.balanceOf.call(bob);

            const bobBalanceExpected = span1.mul(FLOW_RATE)
                .add(span2.mul(FLOW_RATE).mul(toBN(2)));
            const aliceBalanceExpected = INIT_BALANCE
                .sub(bobBalanceExpected);

            assert.equal(alice1Balance.add(aliceDeposit.deposit).toString(), aliceBalanceExpected.toString());
            assert.equal(bobBalance.toString(), bobBalanceExpected.toString());

            await tester.validateSystem();
        });

        it("#1.9 create self flow should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                alice,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> alice")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "FlowAgreement: self flow not allowed");

            await tester.validateSystem();
        });
    });


    describe("#2 FlowAgreement.updateFlow and downgrade", () => {
        it("#2.1 - should downgrade full balance with single flow running", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const createFlowTx = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
            await traveler.advanceTimeAndBlock(ADV_TIME);
            const interimSuperBalanceBob = await superToken.balanceOf.call(bob);
            const bobDowngradeTx = await web3tx(superToken.downgrade, "downgrade all interim balance from bob")(
                interimSuperBalanceBob, {from: bob});
            const tokenBalanceBob = await token.balanceOf.call(bob);
            assert.ok(tokenBalanceBob.eq(INIT_BALANCE.add(interimSuperBalanceBob)),
                "TestToken.balanceOf bob token balance is not correct");
            const superTokenBalanceBob1 = await superToken.balanceOf.call(bob);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            const endBlock = await web3.eth.getBlock("latest");
            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);
            const createFlowTxBlock = await web3.eth.getBlock(createFlowTx.receipt.blockNumber);
            const bobDowngradeTxBlock = await web3.eth.getBlock(bobDowngradeTx.receipt.blockNumber);
            const spanSinceFlowUpdated = endBlock.timestamp - createFlowTxBlock.timestamp;
            const spanSinceBobDowngraded = toBN(endBlock.timestamp - bobDowngradeTxBlock.timestamp);
            const finalSuperBalanceAliceExpected = INIT_BALANCE.sub(toBN(spanSinceFlowUpdated * FLOW_RATE));
            const finalSuperBalanceBobExpected = superTokenBalanceBob1.add(spanSinceBobDowngraded.mul(FLOW_RATE));
            const aliceDeposit = await superToken.realtimeBalanceOf.call(alice, endBlock.timestamp);
            assert.equal(
                finalSuperBalanceAlice.add(aliceDeposit.deposit).toString(),
                finalSuperBalanceAliceExpected.toString(),
                "SuperToken.balanceOf - not correct for alice");
            assert.equal(
                finalSuperBalanceBob.toString(),
                finalSuperBalanceBobExpected.toString(),
                "SuperToken.balanceOf - not correct for bob");

            await tester.validateSystem();
        });

        it("#2.2 - should downgrade partial amount with single flow running", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const txFlow12 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const user2MidwayBalance1 = await superToken.balanceOf.call(bob);
            const user2DowngradeAmount = toBN(user2MidwayBalance1).div(toBN(1000));
            await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
                user2DowngradeAmount.toString(), {from: bob}
            );

            const user2MidwayBalance2Est = INIT_BALANCE.add(user2DowngradeAmount);
            const user2MidwayBalance2 = await token.balanceOf.call(bob);
            assert.equal(
                user2MidwayBalance2.toString(),
                user2MidwayBalance2Est.toString(),
                "Call: TestToken.balanceOf - User 2 token balance is not correct");

            await traveler.advanceTimeAndBlock(ADV_TIME);
            const endBlock = await web3.eth.getBlock("latest");

            const aliceFinalBalance = await superToken.balanceOf.call(alice);
            const user2FinalBalance = await superToken.balanceOf.call(bob);

            const blockFlow12 = await web3.eth.getBlock(txFlow12.receipt.blockNumber);
            const spanFlow12 = endBlock.timestamp - blockFlow12.timestamp;
            const aliceFinalBalanceEst = INIT_BALANCE.sub(toBN(spanFlow12 * FLOW_RATE));
            const user2FinalBalanceEst = (spanFlow12 * FLOW_RATE) - user2DowngradeAmount;
            const aliceDeposit = await superToken.realtimeBalanceOf.call(alice, endBlock.timestamp);

            assert.equal(
                aliceFinalBalance.add(aliceDeposit.deposit).toString(),
                aliceFinalBalanceEst.toString(),
                "Call: SuperToken.balanceOf - not correct for alice");
            assert.equal(
                user2FinalBalance.toString(),
                user2FinalBalanceEst.toString(),
                "Call: SuperToken.balanceOf - not correct for bob");

            await tester.validateSystem();
        });

        it("#2.3 downgrade small portion of tokens with multiple flows running", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: bob});
            await superToken.upgrade(INIT_BALANCE, {from: carol});

            const smallPortion = new toBN(1000);
            const userTokenBalance = await token.balanceOf.call(bob);

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                carol,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> carol")(
                cfa.address,
                dataAgreement,
                {
                    from: bob,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement carol -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: carol,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            await web3tx(superToken.downgrade, "SuperToken.downgrade: User 2 Downgrade")(
                smallPortion, {from: bob});
            const userTokenBalanceFinal = await token.balanceOf.call(bob);

            assert.equal(
                userTokenBalanceFinal.toString(),
                userTokenBalance.add(smallPortion).toString(),
                "User2 downgrade call dont change the token balance");

            await tester.validateSystem();
        });

        it("#2.2 downgrade 1/2 portion of tokens with multiple flows running", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: bob});
            await superToken.upgrade(INIT_BALANCE, {from: carol});

            const halfPortion= new toBN(1000000000000000000);
            const userTokenBalance = await token.balanceOf.call(bob);

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                carol,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> carol")(
                cfa.address,
                dataAgreement,
                {
                    from: bob,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement carol -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: carol,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);


            await web3tx(superToken.downgrade, "SuperToken.downgrade: User 2 Downgrade")(
                halfPortion, {from: bob});

            const userTokenBalanceFinal = await token.balanceOf.call(bob);

            assert.equal(
                userTokenBalanceFinal.toString(),
                userTokenBalance.add(halfPortion).toString(),
                "User2 downgrade call dont change the token balance");

            await tester.validateSystem();
        });

        it("#2.3 downgrade total balance of tokens with multiple flows running", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: bob});
            await superToken.upgrade(INIT_BALANCE, {from: carol});

            const userTokenBalance = await token.balanceOf.call(bob);

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                carol,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> carol")(
                cfa.address,
                dataAgreement,
                {
                    from: bob,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement carol -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: carol,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const userSuperBalance = await superToken.balanceOf.call(bob);
            await web3tx(
                superToken.downgrade,
                "SuperToken.downgrade: User 2 Downgrade"
            )(userSuperBalance, {from: bob});
            const userTokenBalanceFinal = await token.balanceOf.call(bob);

            assert.equal(
                userTokenBalanceFinal.toString(),
                userTokenBalance.add(userSuperBalance).toString(),
                "User2 downgrade call dont change the token balance");

            await tester.validateSystem();
        });
    });

    describe("#3 FlowAgreement.deleteFlow by sender", () => {
        it("#3.1 should stop streaming after deletion with single flow", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            const tx1 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            const tx2 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

            const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            const user1Balance = await superToken.balanceOf.call(alice);
            const user2Balance = await superToken.balanceOf.call(bob);

            const span = block2.timestamp - block1.timestamp;
            const finalUser1 = INIT_BALANCE - (span * FLOW_RATE);
            const finalUser2 = (span * FLOW_RATE);

            assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
            assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");

            await tester.validateSystem();
        });

        it("#3.2 should stop streaming after deletion with multiple flows", async() => {

            await superToken.upgrade(INIT_BALANCE, {from: alice});

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                (FLOW_RATE * 10).toString(),
                "0x"
            ).encodeABI();

            const txFlow12 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                carol,
                (FLOW_RATE).toString(),
                "0x"
            ).encodeABI();

            const txFlow23 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> carol")(
                cfa.address,
                dataAgreement,
                {
                    from: bob,
                }
            );

            dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                dan,
                (FLOW_RATE).toString(),
                "0x"
            ).encodeABI();

            const txFlow24 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> dan")(
                cfa.address,
                dataAgreement,
                {
                    from: bob,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            const txflow12End = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> alice")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);
            const endBlock = await web3.eth.getBlock("latest");
            const user1Balance = await superToken.balanceOf.call(alice);
            const user2Balance = await superToken.balanceOf.call(bob);
            const user3Balance = await superToken.balanceOf.call(carol);
            const user4Balance = await superToken.balanceOf.call(dan);

            const blockFlow12 = await web3.eth.getBlock(txFlow12.receipt.blockNumber);
            const blockFlow23 = await web3.eth.getBlock(txFlow23.receipt.blockNumber);
            const blockFlow24 = await web3.eth.getBlock(txFlow24.receipt.blockNumber);
            const blockFlow12End = await web3.eth.getBlock(txflow12End.receipt.blockNumber);
            const spanFlow12 = blockFlow12End.timestamp - blockFlow12.timestamp;
            const spanFlow23 = endBlock.timestamp - blockFlow23.timestamp;
            const spanFlow24 = endBlock.timestamp - blockFlow24.timestamp;

            const finalUser1 = INIT_BALANCE - (spanFlow12 * FLOW_RATE * 10);
            const finalUser3 = spanFlow23 * FLOW_RATE;
            const finalUser4 = spanFlow24 * FLOW_RATE;
            const finalUser2 = spanFlow12 * FLOW_RATE * 10 - finalUser3 - finalUser4;
            const aliceDeposit = await superToken.realtimeBalanceOf.call(alice, endBlock.timestamp);
            const bobDeposit = await superToken.realtimeBalanceOf.call(bob, endBlock.timestamp);

            let user1Round = ((user1Balance.add(aliceDeposit.deposit)).sub(toBN(finalUser1)));
            const user1ToRound = user1Round.lte(tester.constants.DUST_AMOUNT);
            if(!user1ToRound) {
                user1Round = 0;
            }

            let user2Round = ((user2Balance.add(bobDeposit.deposit)).sub(toBN(finalUser2)));
            const user2ToRound = user2Round.lte(tester.constants.DUST_AMOUNT);
            if(!user2ToRound) {
                user2Round = 0;
            }

            assert.equal(
                user1Balance.add(aliceDeposit.deposit).toString(),
                toBN(finalUser1).add(user1Round).toString(),
                "User 1 Final balance is wrong");
            assert.equal(
                user2Balance.add(bobDeposit.deposit).toString(),
                toBN(finalUser2).add(user2Round).toString(),
                "User 2 Final balance is wrong");
            assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wrong");
            assert.equal(user4Balance.toString(), finalUser4.toString(), "User 4 Final balance is wrong");

            await tester.validateSystem();
        });
    });

    describe("#4 FlowAgreement.deleteFlow by receiver", () => {
        it("#4.1 should stop streaming after deletion with single flow", async() => {

            await superToken.upgrade(INIT_BALANCE, {from: alice});

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                (FLOW_RATE).toString(),
                "0x"
            ).encodeABI();

            const tx1 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            const tx2 = await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

            const user1Balance = await superToken.balanceOf.call(alice);
            const user2Balance = await superToken.balanceOf.call(bob);

            const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            const span = block2.timestamp - block1.timestamp;
            const finalUser1 = INIT_BALANCE - (span * FLOW_RATE);
            const finalUser2 = (span * FLOW_RATE);

            assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
            assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");

            await tester.validateSystem();
        });
    });

    describe("#5 FlowAgreement.deleteFlow by liquidator", () => {
        it("#5.1 liquidation of insolvent account should succeed", async() => {

            await superToken.upgrade(toWad(10), {from : alice});

            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();
            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME * 5);

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();
            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement admin -> alice")(
                cfa.address,
                dataAgreement,
                {
                    from: admin,
                }
            );

            const flowRate = (await cfa.getFlow.call(
                superToken.address,
                alice, bob))[3].toString();

            assert.equal(flowRate.toString(), "0", "Liquidation didn't happen");
        });

        it("#5.2 liquidation of solvent account should fail", async () => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );
            await traveler.advanceTimeAndBlock(ADV_TIME);

            dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: admin,
                    }
                ), "FlowAgreement: account is solvent");
        });

        it("#5.3 liquidation of non existing flow should fail", async () => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            let dataAgreement = cfa.contract.methods.deleteFlow(
                superToken.address,
                alice,
                bob,
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "FlowAgreement: flow does not exist");
        });

    });


    describe("#6 FlowAgreement Deposit and OwedDeposit", () => {
        it("#6.1 Should fail if sender don't have balance to pay deposit", async() => {
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                "100000000000",
                "0x"
            ).encodeABI();

            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "CFA: not enough available balance");
        });

        it("#6.2 Should fail if sender have small balance to pay deposit", async() => {
            await superToken.upgrade(toWad(1), {from : alice});
            let dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();
            await expectRevert(
                web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                    cfa.address,
                    dataAgreement,
                    {
                        from: alice,
                    }
                ), "CFA: not enough available balance");
        });
    });
});
