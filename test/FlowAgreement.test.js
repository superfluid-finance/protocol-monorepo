const { expectRevert } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");

const traveler = require("ganache-time-traveler");

const Tester = require("./Tester");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);

contract("Flow Agreement", accounts => {

    const tester = new Tester(accounts.slice(0, 5));
    const { admin, alice, bob, carol, dan } = tester.aliases;
    const { INIT_BALANCE } = tester.constants;

    let token;
    let superToken;
    let flowAgreement;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            token,
            superToken,
            flowAgreement
        } = tester.contracts);
    });

    describe("#1 FlowAgreement.updateFlow", () => {
        it("#1.1 should stream in correct flow rate with single flow", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});

            const tx = await web3tx(
                flowAgreement.updateFlow,
                "updateFlow: alice -> bob"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

            // test flow views
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, alice, bob
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, bob, alice
            )).toString(), "0");

            const beginBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);
            const endBlock = await web3.eth.getBlock("latest");

            const aliceBalance = await superToken.balanceOf.call(alice);
            const bobBalance = await superToken.balanceOf.call(bob);
            const span = endBlock.timestamp - beginBlock.timestamp;
            const aliceBalanceExpected = INIT_BALANCE - (span * FLOW_RATE);
            const bobBalanceExpected = (span * FLOW_RATE);

            assert.equal(aliceBalance.toString(), aliceBalanceExpected.toString(),
                "Alice final balance is wrong");
            assert.equal(bobBalance.toString(), bobBalanceExpected.toString(),
                "Bob final balance is wrong");

            await tester.validateSystem();
        });

        it("#1.2 should stream in correct flow rate after second update with single flow", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});

            const tx = await web3tx(
                flowAgreement.updateFlow,
                "updateFlow: alice -> bob"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

            // test flow views
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, alice, bob
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, bob, alice
            )).toString(), "0");

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const tx2 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: alice -> carol"
            )(superToken.address, alice, carol, FLOW_RATE, {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);

            // test flow views
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, alice
            )).toString(), FLOW_RATE.mul(toBN(-2)).toString());
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, bob
            )).toString(), FLOW_RATE.toString());
            assert.equal((await flowAgreement.getNetFlow.call(
                superToken.address, carol
            )).toString(), FLOW_RATE.toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, alice, bob
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, bob, alice
            )).toString(), "0");
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, alice, carol
            )).toString(), FLOW_RATE.mul(toBN(-1)).toString());
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, carol, alice
            )).toString(), "0");
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, bob, carol
            )).toString(), "0");
            assert.equal((await flowAgreement.getFlow.call(
                superToken.address, carol, bob
            )).toString(), "0");

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

            assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
            assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
            assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wring");

            await tester.validateSystem();
        });

        it("#1.3 update with negative flow rate should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            await expectRevert(
                web3tx(flowAgreement.updateFlow, "creating with negative flow")(
                    superToken.address, alice, bob, "-10000000000000000000000", {from: alice}
                ), "FlowAgreement: negative flow rate not allowed");
        });

        it("#1.4 update active flow to negative flow rate should also fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            await flowAgreement.updateFlow(superToken.address, alice, bob, toWad(1), {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);
            await expectRevert(
                web3tx(flowAgreement.updateFlow, "creating with negative flow")(
                    superToken.address, alice, bob, FLOW_RATE.mul(toBN(-1)), {from: alice}
                ), "FlowAgreement: negative flow rate not allowed");

            await tester.validateSystem();
        });

        it("#1.5 update other's flow should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            await flowAgreement.updateFlow(superToken.address, alice, bob, toWad(1), {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);
            await expectRevert(
                web3tx(flowAgreement.updateFlow, "carol creating flow for alice should fail")(
                    superToken.address, alice, bob, toWad(1), {from: carol}
                ), "FlowAgreement: only sender can update its own flow");

            await tester.validateSystem();
        });

        it("#1.6 update with zero rate should fail", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            await flowAgreement.updateFlow(superToken.address, alice, bob, toWad(1), {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);
            await expectRevert(
                web3tx(flowAgreement.updateFlow, "creating with negative flow")(
                    superToken.address, alice, bob, 0, {from: alice}
                ), "FlowAgreement: use delete flow");

            await tester.validateSystem();
        });

        it("#1.7 should allow net flow rate 0 then back to normal rate", async() => {
            await superToken.upgrade(INIT_BALANCE, {from: alice});
            await superToken.upgrade(INIT_BALANCE, {from: carol});

            const tx1 = await web3tx(
                flowAgreement.updateFlow,
                "updateFlow: alice -> bob"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);

            const tx2 = await web3tx(
                flowAgreement.updateFlow,
                "updateFlow: carol -> alice"
            )(superToken.address, carol, alice, FLOW_RATE, {from: carol});
            const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);

            const tx3 = await web3tx(
                flowAgreement.updateFlow,
                "updateFlow: alice -> dan"
            )(superToken.address, alice, dan, FLOW_RATE, {from: alice});
            const block3 = await web3.eth.getBlock(tx3.receipt.blockNumber);
            await traveler.advanceTimeAndBlock(ADV_TIME);

            const endBlock = await web3.eth.getBlock("latest");

            const span1 = toBN(block2.timestamp - block1.timestamp);
            const span2 = toBN(block3.timestamp - block2.timestamp);
            const span3 = toBN(endBlock.timestamp - block3.timestamp);

            const alice1Balance = await superToken.balanceOf.call(alice);
            const bobBalance = await superToken.balanceOf.call(bob);
            const carolBalance = await superToken.balanceOf.call(carol);
            const danBalance = await superToken.balanceOf.call(dan);

            const aliceBalanceExpected = INIT_BALANCE
                .sub(span1.mul(FLOW_RATE))
                .sub(span3.mul(FLOW_RATE));
            const bobBalanceExpected = span1.add(span2).add(span3).mul(FLOW_RATE);
            const carolBalanceExpected = INIT_BALANCE
                .sub(span2.add(span3).mul(FLOW_RATE));
            const danBalanceExpected = span3.mul(FLOW_RATE);

            assert.equal(alice1Balance.toString(), aliceBalanceExpected.toString());
            assert.equal(bobBalance.toString(), bobBalanceExpected.toString());
            assert.equal(carolBalance.toString(), carolBalanceExpected.toString());
            assert.equal(danBalance.toString(), danBalanceExpected.toString());

            await tester.validateSystem();
        });
    });

    describe("#2 FlowAgreement.updateFlow and downgrade", () => {
        it("#2.1 - should downgrade full balance with single flow running", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});

            const updateFlowTx = await web3tx(flowAgreement.updateFlow, "updateFlow alice to bob")(
                superToken.address, alice, bob, FLOW_RATE, {from: alice});
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

            const updateFlowTxBlock = await web3.eth.getBlock(updateFlowTx.receipt.blockNumber);
            const bobDowngradeTxBlock = await web3.eth.getBlock(bobDowngradeTx.receipt.blockNumber);
            const spanSinceFlowUpdated = endBlock.timestamp - updateFlowTxBlock.timestamp;
            const spanSinceBobDowngraded = toBN(endBlock.timestamp - bobDowngradeTxBlock.timestamp);

            const finalSuperBalanceAliceExpected = INIT_BALANCE.sub(toBN(spanSinceFlowUpdated * FLOW_RATE));
            const finalSuperBalanceBobExpected = superTokenBalanceBob1.add(spanSinceBobDowngraded.mul(FLOW_RATE));

            assert.equal(
                finalSuperBalanceAlice.toString(),
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

            const txFlow12 = await web3tx(flowAgreement.updateFlow, "FlowAgreement.updateFlow alice to bob")(
                superToken.address, alice, bob, FLOW_RATE, {from: alice});

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

            assert.equal(
                aliceFinalBalance.toString(),
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

            const smallPortion = new toBN(1000);
            const userTokenBalance = await token.balanceOf.call(bob);

            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 1 -> User 2 Create new Flow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 2 -> User 3 Create new Flow"
            )(superToken.address, bob, carol, FLOW_RATE, {from: bob});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateflow: User 3 -> User 2 Create new Flow"
            )(superToken.address, carol, bob, FLOW_RATE, {from: carol});

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

            const halfPortion= new toBN(1000000000000000000);
            const userTokenBalance = await token.balanceOf.call(bob);

            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 1 -> User 2 Create new Flow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 2 -> User 3 Create new Flow"
            )(superToken.address, bob, carol, FLOW_RATE, {from: bob});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateflow: User 3 -> User 2 Create new Flow"
            )(superToken.address, carol, bob, FLOW_RATE, {from: carol});

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

            const userTokenBalance = await token.balanceOf.call(bob);

            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 1 -> User 2 Create new Flow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 2 -> User 3 Create new Flow"
            )(superToken.address, bob, carol, FLOW_RATE, {from: bob});
            await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 3 -> User 2 Create new Flow"
            )(superToken.address, carol, bob, FLOW_RATE, {from: carol});

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

            const tx1 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 1 -> User 2 Create new Flow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const tx2 = await web3tx(
                flowAgreement.deleteFlow,
                "FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
            )(superToken.address, alice, bob, {from: alice});

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

            const txFlow12 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 1 -> User 2 Update new Flow"
            )(superToken.address, alice, bob, (FLOW_RATE * 10).toString(), {from: alice});

            const txFlow23 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 2 -> User 3 Create new Flow"
            )(superToken.address, bob, carol, FLOW_RATE, {from: bob});

            const txFlow24 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.updateFlow: User 2 -> User 4 Create new Flow"
            )(superToken.address, bob, dan, FLOW_RATE, {from: bob});

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const txflow12End = await web3tx(
                flowAgreement.deleteFlow,
                "FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
            )(superToken.address, alice, bob, {from: alice});

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

            assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
            assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
            assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wrong");
            assert.equal(user4Balance.toString(), finalUser4.toString(), "User 4 Final balance is wrong");

            await tester.validateSystem();
        });
    });

    describe("#4 FlowAgreement.deleteFlow by receiver", () => {
        it("#4.1 should stop streaming after deletion with single flow", async() => {

            await superToken.upgrade(INIT_BALANCE, {from: alice});

            const tx1 = await web3tx(
                flowAgreement.updateFlow,
                "FlowAgreement.UpdateFlow: User 1 -> User 2 Create new Flow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const tx2 = await web3tx(
                flowAgreement.deleteFlow,
                "FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
            )(superToken.address, alice, bob, {from: alice});

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

            await superToken.upgrade(toWad(1), {from : alice});

            await web3tx(flowAgreement.updateFlow, "FlowAgreement.updateFlow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

            await traveler.advanceTimeAndBlock(ADV_TIME);

            await web3tx(
                flowAgreement.deleteFlow,
                "FlowAgreement.deleteFlow: User 1 will be liquidated"
            )(superToken.address, alice, bob, {from: admin});

            const flowRate = await flowAgreement.getFlow.call(superToken.address, alice, bob);

            assert.equal(flowRate.toString(), "0", "Liquidation didn't happen");
        });

        it("#5.2 liquidation of solvent account should fail", async () => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            await flowAgreement.updateFlow(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);

            await expectRevert(
                web3tx(flowAgreement.deleteFlow, "FlowAgreement.deleteFlow by liquidator")(
                    superToken.address,
                    alice,
                    bob, {
                        from: admin
                    }
                ), "FlowAgreement: account is solvent");
        });

        it("#5.3 liquidation of non existing flow should fail", async () => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);

            await expectRevert(
                web3tx(flowAgreement.deleteFlow, "FlowAgreement.deleteFlow non existing flow by alice")(
                    superToken.address,
                    alice,
                    bob, {
                        from: alice
                    }
                ), "FlowAgreement: flow does not exist");
        });
    });
});
