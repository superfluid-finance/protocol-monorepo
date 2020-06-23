const Tester = require("./Tester");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-helpers");

const traveler = require("ganache-time-traveler");
const toBN = web3.utils.toBN;

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);
const FLOW_RATE_ADDITIONAL = toWad(2);

contract("Super Token", accounts => {

    const tester = new Tester(accounts.slice(0, 3));
    const { alice, bob } = tester.aliases;
    const { INIT_BALANCE } = tester.constants;

    let token;
    let superToken;
    let flowAgreement;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async () => {
        await tester.resetContracts();
        ({
            token,
            superToken,
            flowAgreement
        } = tester.contracts);
    });

    afterEach(async () => {
        await tester.validateSystem();
    });

    it("#1 - realtimeBalanceOf current block time equals balaceOf", async() => {

        // TODO some actual balance please

        let balanceOf = await superToken.balanceOf.call(alice);
        const {timestamp} = await web3.eth.getBlock("latest");
        let realBalanceOf = await superToken.realtimeBalanceOf.call(alice, timestamp);

        assert.ok(balanceOf.eq(realBalanceOf), "BalanceOf should be the same as realtimeBalanceOf");
    });

    it("#2 - upgrading to SuperToken", async () => {

        let initialBalance = await token.balanceOf.call(alice);

        await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - from alice") (
            toWad(2), {
                from: alice
            });

        let finalBalance = await token.balanceOf.call(alice);
        let finalSuperTokenBalance = await superToken.balanceOf.call(alice);

        assert.isOk(initialBalance.gt(finalBalance), "Call: TestToken.balanceOf - is wrong");
        assert.equal(finalSuperTokenBalance.toString(), "2000000000000000000", "Call: SuperToken.balanceOf - is wrong");
    });

    it("#2.1 - downgrading from SuperToken by single account", async() => {

        let initialBalance = await token.balanceOf.call(alice);

        await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - from alice") (
            toWad(2), {
                from: alice
            });


        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from alice") (
            toWad(2), {
                from: alice
            });


        let finalBalance = await token.balanceOf.call(alice);
        let finalSuperTokenBalance = await superToken.balanceOf.call(alice);

        assert.isOk(initialBalance.toString(), finalBalance.toString(), "Call: TestToken.balanceOf - is wrong");
        assert.equal(finalSuperTokenBalance.toString(), "0", "Call: SuperToken.balanceOf - is wrong");
    });


    it("#2.2 - downgrading from SuperToken by multiple accounts", async () => {

        let initialBalanceUser1 = await token.balanceOf.call(alice);
        let initialSuperBalanceUser1 = await superToken.balanceOf.call(alice);

        await superToken.upgrade(toWad(2), {from: alice});
        await superToken.upgrade(toWad(1), {from: bob});

        let initialSuperBalanceUser2 = await superToken.balanceOf.call(bob);

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from alice") (
            toWad(2), {
                from: alice
            });

        let finalBalanceUser1 = await token.balanceOf.call(alice);
        let finalSuperBalanceUser1 = await superToken.balanceOf.call(alice);
        let finalSuperBalanceUser2 = await superToken.balanceOf.call(bob);

        assert.equal(
            initialBalanceUser1.toString(),
            finalBalanceUser1.toString(),
            "Call: TestToken.balanceOf - not correct for user 1");
        assert.equal(
            initialSuperBalanceUser1.toString(),
            finalSuperBalanceUser1.toString(),
            "Call: SuperToken.balanceOf - not correct for user 1");
        assert.equal(
            initialSuperBalanceUser2.toString(),
            finalSuperBalanceUser2.toString(),
            "Call: SuperToken.balanceOf - not correct for user 2");
    });

    it("#2.3 - downgrade of full balance with flows running", async() => {

        await superToken.upgrade(INIT_BALANCE, {from : alice});  //10 tokens

        await superToken.balanceOf.call(alice);
        let tx1 = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
        await superToken.balanceOf.call(alice);

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let result2 = await superToken.balanceOf.call(bob);

        let tx2 = await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
            result2, {
                from: bob
            }
        );

        let finalBalance = INIT_BALANCE.add(result2);
        let userTokenBalance = await token.balanceOf.call(bob);

        assert.ok(
            userTokenBalance.eq(finalBalance),
            "Call: TestToken.balanceOf - User 2 token balance is not correct");


        let slippage = await superToken.balanceOf.call(bob);
        if (slippage > 0) {
            console.warn("Detected blockchain time inconsistancy");
        }

        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let result3 = await superToken.balanceOf.call(alice);
        let result4 = await superToken.balanceOf.call(bob);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span1 = endBlock.timestamp - block1.timestamp;
        let span2 = web3.utils.toBN(endBlock.timestamp - block2.timestamp);

        let final1 = INIT_BALANCE.sub(web3.utils.toBN(span1 * FLOW_RATE));
        let final2 = slippage.add(span2.mul(FLOW_RATE));

        assert.equal(
            result3.toString(),
            final1.toString(),
            "Call: SuperToken.balanceOf - not correct for alice");
        assert.equal(
            result4.toString(),
            final2.toString(),
            "Call: SuperToken.balanceOf - not correct for bob");
    });

    it("#2.4 - partial amount downgraded with flows running", async() => {
        await superToken.upgrade(INIT_BALANCE, {from : alice});

        let txFlow12 = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let user2MidwayBalance1 = await superToken.balanceOf.call(bob);
        let user2DowngradeAmount = web3.utils.toBN(user2MidwayBalance1 / 1000);
        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
            user2DowngradeAmount.toString(), {
                from: bob
            }
        );

        let user2MidwayBalance2Est = INIT_BALANCE.add(user2DowngradeAmount);
        let user2MidwayBalance2 = await token.balanceOf.call(bob);
        assert.equal(
            user2MidwayBalance2.toString(),
            user2MidwayBalance2Est.toString(),
            "Call: TestToken.balanceOf - User 2 token balance is not correct");

        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let aliceFinalBalance = await superToken.balanceOf.call(alice);
        let user2FinalBalance = await superToken.balanceOf.call(bob);

        const blockFlow12 = await web3.eth.getBlock(txFlow12.receipt.blockNumber);
        let spanFlow12 = endBlock.timestamp - blockFlow12.timestamp;
        let aliceFinalBalanceEst = INIT_BALANCE.sub(web3.utils.toBN(spanFlow12 * FLOW_RATE));
        let user2FinalBalanceEst = (spanFlow12 * FLOW_RATE) - user2DowngradeAmount;

        assert.equal(
            aliceFinalBalance.toString(),
            aliceFinalBalanceEst.toString(),
            "Call: SuperToken.balanceOf - not correct for alice");
        assert.equal(
            user2FinalBalance.toString(),
            user2FinalBalanceEst.toString(),
            "Call: SuperToken.balanceOf - not correct for bob");
    });

    it("#3 - Check Agreement Data layout", async() => {

        await superToken.upgrade(INIT_BALANCE, {from : alice});  //10 tokens
        let tx = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        let flowId = web3.utils.soliditySha3(alice, bob);
        let opposedFlowId = web3.utils.soliditySha3(bob, alice);
        let resultFlowId = await superToken.getAgreementData.call(flowAgreement.address, flowId);
        let resultOpposedFlowId = await superToken.getAgreementData.call(flowAgreement.address, opposedFlowId);

        assert.ok(resultOpposedFlowId == null, "User Account Data is not empty");

        let split = web3.eth.abi.decodeParameters(["uint256", "int256"], resultFlowId);

        const block1 = await web3.eth.getBlock(tx.receipt.blockNumber);

        assert.equal(split[0], block1.timestamp, "User 1 to User 2 wrong timestamp");
        assert.equal(split[1], FLOW_RATE * -1, "User 1 Flow Rate wrong");

    });

    it("#4 - Check ActiveAgreementClass Register - multiple additions", async() => {

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
    });

    it("#5 - Check AgreementState - assert that is saved with the correct data", async() => {

        let tx = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        let stateUser1 = await superToken.getAgreementAccountState.call(flowAgreement.address, alice);
        let stateUser2 = await superToken.getAgreementAccountState.call(flowAgreement.address, bob);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        assert.ok(splitUser1[0] == block.timestamp, "User 1 timestamp in State is wrong");
        assert.equal(splitUser1[1], -1 * FLOW_RATE, "User 1 Flow Rate is wrong");

        assert.ok(splitUser2[0] == block.timestamp, "User 2 timestamp in State is wrong");
        assert.equal(splitUser2[1], FLOW_RATE, "User 2 Flow Rate is wrong");
    });

    it("#5.1 - Check AgreementState - assert that State is updated, but the counters stay the same", async() => {

        await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
        let tx = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        let stateUser1 = await superToken.getAgreementAccountState.call(flowAgreement.address, alice);
        let stateUser2 = await superToken.getAgreementAccountState.call(flowAgreement.address, bob);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.ok(splitUser1[0] == block.timestamp, "User 1 timestamp in State is wrong");
        assert.equal(splitUser1[1], (-1 * FLOW_RATE) * 2, "User 1 Flow Rate is wrong");

        assert.ok(splitUser2[0] == block.timestamp, "User 2 timestamp in State is wrong");
        assert.equal(splitUser2[1], FLOW_RATE * 2, "User 2 Flow Rate is wrong");
    });

    it("#5.2 - Check AgreementState - assert that State is updated by using the updateFlow x2", async() => {

        await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
        let tx = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement."
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        let stateUser1 = await superToken.getAgreementAccountState.call(flowAgreement.address, alice);
        let stateUser2 = await superToken.getAgreementAccountState.call(flowAgreement.address, bob);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.ok(splitUser1[0] == block.timestamp, "User 1 timestamp in State is wrong");
        assert.equal(splitUser1[1], (-1 * FLOW_RATE) * 2, "User 1 Flow Rate is wrong");

        assert.ok(splitUser2[0] == block.timestamp, "User 2 timestamp in State is wrong");
        assert.equal(splitUser2[1], FLOW_RATE * 2, "User 2 Flow Rate is wrong");
    });

    it("#6 - Snapshot of Balance - assert passed balance after an update", async() => {

        await superToken.upgrade(INIT_BALANCE, {from : alice});
        let tx1 = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow")(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        let snapshot1 = await superToken.getSettledBalance.call(alice);
        let snapshot2 = await superToken.getSettledBalance.call(bob);

        assert.equal(snapshot1.toString(), INIT_BALANCE.toString(), "Call: SuperToken.getSnapshot user 1 is incorrect");
        assert.equal(snapshot2, 0, "Call: SuperToken.getSnapshot user 2 is incorrect");

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let tx2 = await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        snapshot1 = await superToken.getSettledBalance.call(alice);
        snapshot2 = await superToken.getSettledBalance.call(bob);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span1 = block2.timestamp - block1.timestamp;
        let result1 = FLOW_RATE * span1;
        const checkUser1 = INIT_BALANCE.sub(toBN(result1));

        assert.equal(snapshot1.toString(), checkUser1.toString(),
            "Call: SuperToken.getSnapshot first call Alice is incorrect"
        );
        assert.equal(snapshot2.toString(), result1, "Call: SuperToken.getSnapshot first call Bob is incorrect");

        await traveler.advanceTimeAndBlock(ADV_TIME);
        let tx3 = await web3tx(
            flowAgreement.deleteFlow,
            "Call FlowAgreement.deleteFlow"
        )(superToken.address, alice, bob, {from: alice});

        let snapshot3 = await superToken.getSettledBalance.call(alice);
        let snapshot4 = await superToken.getSettledBalance.call(bob);

        const block3 = await web3.eth.getBlock(tx3.receipt.blockNumber);
        let span2 = (block3.timestamp - block2.timestamp) + (block3.timestamp - block1.timestamp);
        let result2 = (span2 * FLOW_RATE);
        const check2User1 = INIT_BALANCE.sub(toBN(result2));

        assert.equal(
            snapshot3.toString(),
            check2User1.toString(),
            "Call: SuperToken.getSnapshot second call Alice is incorrect");
        assert.equal(
            snapshot4.toString(),
            result2.toString(),
            "Call: SuperToken.getSnapshot second call Bob is incorrect");
    });

    it("#6.1 - Snapshot of Balance - assert that is not time dependancy", async() => {
        await superToken.upgrade(INIT_BALANCE, {from : alice});
        await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        await web3tx(
            flowAgreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, alice, bob, FLOW_RATE, {from: alice});
        let snapshot1 = await superToken.getSettledBalance.call(bob);

        await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

        let snapshot2 = await superToken.getSettledBalance.call(bob);

        assert.equal(snapshot1.toString(), snapshot2.toString(), "Snapshot change with time");
    });
});
