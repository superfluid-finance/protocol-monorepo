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
const FLOW_RATE_ADDITIONAL = toWad(2);

contract("Super Token", accounts => {

    const tester = new Tester(accounts.slice(0, 4));
    const { alice, bob, carol } = tester.aliases;
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

    describe("#1 SuperToken.upgrade", () => {
        it("#1.1 - should upgrade if enough balance", async () => {
            const initialBalance = await token.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2.0 tokens from alice") (
                toWad(2), {
                    from: alice
                });
            const { timestamp } = await web3.eth.getBlock("latest");

            const finalBalance = await token.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);
            const finalRealBalance = await superToken.realtimeBalanceOf.call(alice, timestamp);

            assert.equal(finalSuperTokenBalance.toString(), toWad(2).toString(),
                "SuperToken.balanceOf is wrong");
            assert.equal(initialBalance.sub(finalBalance).toString(), toWad(2).toString(),
                "SuperToken.upgrade should manage underlying tokens");
            assert.ok(finalRealBalance.eq(finalSuperTokenBalance),
                "balanceOf should equal realtimeBalanceOf");

            await tester.validateSystem();
        });

        it("#1.2 - should not upgrade without enough underlying balance", async() => {
            const initialBalance = await token.balanceOf.call(alice);
            await expectRevert(web3tx(superToken.upgrade, "Call: SuperToken.upgrade - bad balance")(
                initialBalance.add(toBN(1)), {from: alice}), "ERC20: transfer amount exceeds balance");
            await tester.validateSystem();
        });
    });

    describe("#2 SuperToken.downgrade", () => {
        it("#2.1 - should downgrade by single account", async() => {
            const initialBalance = await token.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            await web3tx(superToken.downgrade, "SuperToken.downgrade 2 from alice") (
                toWad(1), {
                    from: alice
                });

            const finalBalance = await token.balanceOf.call(alice);
            const finalSuperTokenBalance = await superToken.balanceOf.call(alice);

            assert.isOk(initialBalance.sub(finalBalance).toString(), toWad(1),
                "TestToken.balanceOf should recover after downgrade");
            assert.equal(finalSuperTokenBalance.toString(), toWad("1"),
                "SuperToken.balanceOf is wrong");

            await tester.validateSystem();
        });

        it("#2.2 - should downgrade by multiple accounts", async () => {
            const initialBalanceAlice = await token.balanceOf.call(alice);
            const initialSuperBalanceAlice = await superToken.balanceOf.call(alice);

            await web3tx(superToken.upgrade, "upgrade 2 from alice")(toWad(2), {from: alice});
            await web3tx(superToken.upgrade, "upgrade 1 from bob")(toWad(1), {from: bob});

            const initialSuperBalanceBob = await superToken.balanceOf.call(bob);

            await web3tx(superToken.downgrade, "downgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });

            const finalBalanceAlice = await token.balanceOf.call(alice);
            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(
                initialBalanceAlice.toString(),
                finalBalanceAlice.toString(),
                "TestToken.balanceOf - not correct for alice");
            assert.equal(
                initialSuperBalanceAlice.toString(),
                finalSuperBalanceAlice.toString(),
                "Call: SuperToken.balanceOf - not correct for user 1");
            assert.equal(
                initialSuperBalanceBob.toString(),
                finalSuperBalanceBob.toString(),
                "Call: SuperToken.balanceOf - not correct for user 2");

            await tester.validateSystem();
        });

        it("#2.3 - should downgrade full balance with flows running", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});

            const updateFlowTx = await web3tx(flowAgreement.updateFlow, "updateFlow alice to bob")(
                superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);

            const interimSuperBalanceBob = await superToken.balanceOf.call(bob);
            await expectRevert(
                web3tx(superToken.downgrade, "downgrade all(+1) from bob should fail")(
                    interimSuperBalanceBob.add(toBN(1)), {from: bob}
                ), "SuperToken: downgrade amount exceeds balance");
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
            const spanSinceBobDowngraded = web3.utils.toBN(endBlock.timestamp - bobDowngradeTxBlock.timestamp);

            const finalSuperBalanceAliceExpected = INIT_BALANCE.sub(web3.utils.toBN(spanSinceFlowUpdated * FLOW_RATE));
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

        it("#2.4 - should downgrade partial amount with flows running", async() => {
            await superToken.upgrade(INIT_BALANCE, {from : alice});

            const txFlow12 = await web3tx(flowAgreement.updateFlow, "FlowAgreement.updateFlow alice to bob")(
                superToken.address, alice, bob, FLOW_RATE, {from: alice});

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const user2MidwayBalance1 = await superToken.balanceOf.call(bob);
            const user2DowngradeAmount = web3.utils.toBN(user2MidwayBalance1 / 1000);
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
            const aliceFinalBalanceEst = INIT_BALANCE.sub(web3.utils.toBN(spanFlow12 * FLOW_RATE));
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

        it("#2.5 - should not downgrade if there is no balance", async () => {
            await expectRevert(web3tx(superToken.downgrade, "Call: SuperToken.downgrade - bad balance")(
                toBN(1), {
                    from: alice
                }), "SuperToken: downgrade amount exceeds balance");
        });
    });

    describe("#3 SuperToken ISuperAgreementStorage(TBD) operations", () => {
        // TODO To be improved with a mock agreement class

        it("#3.1 - should track active agreement classes", async() => {

            await web3tx(
                flowAgreement.updateFlow,
                "Call: FlowAgreement.updateFlow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            let aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
            let user2AgreementClasses = await superToken.getAccountActiveAgreements.call(bob);

            assert.ok(aliceAgreementClasses.length == 1, "User 1 number of ActiveAgreementClasses is wrong");
            assert.ok(user2AgreementClasses.length == 1, "User 2 number of ActiveAgreementClasses is wrong");
            assert.equal(aliceAgreementClasses[0], flowAgreement.address, "User 1 ActiveAgreementClass is wrong");
            assert.equal(user2AgreementClasses[0], flowAgreement.address, "User 2 ActiveAgreementClass is wrong");

            await web3tx(
                flowAgreement.updateFlow,
                "Call: FlowAgreement.updateFlow"
            )(superToken.address, bob, alice, FLOW_RATE_ADDITIONAL, {from: bob});

            aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
            user2AgreementClasses = await superToken.getAccountActiveAgreements.call(bob);

            assert.ok(aliceAgreementClasses.length == 1, "User 1 number of ActiveAgreementClasses is wrong");
            assert.ok(user2AgreementClasses.length == 1, "User 2 number of ActiveAgreementClasses is wrong");
            assert.equal(aliceAgreementClasses[0], flowAgreement.address, "User 1 ActiveAgreementClass is wrong");
            assert.equal(user2AgreementClasses[0], flowAgreement.address, "User 2 ActiveAgreementClass is wrong");

            await web3tx(
                flowAgreement.updateFlow,
                "Call: FlowAgreement.updateFlow"
            )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
            aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
            user2AgreementClasses = await superToken.getAccountActiveAgreements.call(bob);

            assert.ok(aliceAgreementClasses.length == 0, "User 1 number of ActiveAgreementClasses is wrong");
            assert.ok(user2AgreementClasses.length == 0, "User 2 number of ActiveAgreementClasses is wrong");

            await tester.validateSystem();
        });

        it("#3.2 - should only be updated by authorized agreement", async () => {
            await expectRevert(
                web3tx(superToken.updateAgreementAccountState,
                    "SuperToken.updateAgreementAccountState by alice directly")(
                    alice,
                    "0x42", {from: alice}
                ), "SuperToken: unauthorized agreement storage access");
        });
    });

    describe("#4 SuperToken.transfer", () => {
        it("#4.1 - should transfer available amount", async() => {
            await web3tx(superToken.upgrade, "SuperToken.upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await web3tx(superToken.transfer, "SuperToken.transfer 2 from alice to bob") (
                bob, toWad(0.5), {
                    from: alice
                });

            const finalSuperBalanceAlice = await superToken.balanceOf.call(alice);
            const finalSuperBalanceBob = await superToken.balanceOf.call(bob);

            assert.equal(finalSuperBalanceAlice.toString(), toWad(1.5));
            assert.equal(finalSuperBalanceBob.toString(), toWad(0.5));

            await tester.validateSystem();
        });

        it("#4.2 - should not transfer unavailable balance", async() => {
            await web3tx(superToken.upgrade, "upgrade 2 from alice") (
                toWad(2), {
                    from: alice
                });
            await expectRevert(
                web3tx(superToken.transfer, "transfer 2(+1wei) from alice to bob should fail")(
                    bob, toWad(2).add(toBN(1)), {from: alice}
                ), "transfer amount exceeds balance");
            await tester.validateSystem();
        });

        it("#4.3 - should be able to transfer flow balance", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});

            await web3tx(flowAgreement.updateFlow, "updateFlow alice to bob")(
                superToken.address, alice, bob, FLOW_RATE, {from: alice});
            await traveler.advanceTimeAndBlock(ADV_TIME);

            const superBalanceBob = await superToken.balanceOf.call(bob);
            await expectRevert(
                web3tx(superToken.transfer, "downgrade all(+1) from bob should fail")(
                    carol, superBalanceBob.add(toBN(1)), {from: bob}
                ), "transfer amount exceeds balance.");
            await web3tx(superToken.transfer, "downgrade all interim balance from bob to carol")(
                carol, superBalanceBob, {from: bob});

            const superBalanceCarol = await superToken.balanceOf.call(carol);
            assert.equal(superBalanceCarol.toString(), superBalanceBob.toString());

            await tester.validateSystem();
        });
    });

    describe("#5 SuperToken.approve", () => {
        // TODO
    });

});
