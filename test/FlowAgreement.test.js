const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");
const InstruSuperToken = artifacts.require("InstruSuperToken");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

const ADV_TIME = 2;
const FLOW_RATE = toWad(2);
const SECONDARY_FLOW_RATE = toWad(1);
const FLOW_RATE_UPDATED = toWad(3);

contract("Flow Agreement", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];
    const user3 = accounts[3];

    let token;
    let superToken;
    let agreement;
    let superTokenDebug;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);
    });

    beforeEach(async () => {

        agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")(
            {
                from:admin
            });

        token = await web3tx(ERC20Mintable.new, "Call: ERC20Mintable.new")(
            {
                from: admin
            });

        await token.mint(user1, toWad(10));
        await token.mint(user2, toWad(10));

        superToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        superTokenDebug = await web3tx(InstruSuperToken.new, "Call InstruSuperToken.new")(
            superToken.address
        );

        await web3tx(token.approve, "Call: token.approve from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );

        await web3tx(token.approve, "Call: token.approve from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );
    });

    it("Create a new flow", async () => {

        let tx = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - User1 -> User2 new Agreement")(
            superToken.address,
            user1,
            user2,
            100, {
                from: user1
            }
        );


        let stateUser1 = await superToken.getState.call(agreement.address, user1);
        let stateUser2 = await superToken.getState.call(agreement.address, user2);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.equal(splitUser1[0], tx.timestamp, "Call: SuperToken.getState - User1 start date dont match");
        assert.equal(splitUser1[1], -100, "Call: SuperToken.getState - User 1 Flow Rate incorrect");

        assert.equal(splitUser2[0], tx.timestamp, "Call: SuperToken.getState - User2 start date dont match");
        assert.equal(splitUser2[1], 100, "Call: SuperToken.getState - User2 Flow Rate incorrect");

    });

    it("Super Balance", async() => {

        await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - From user1") (
            toWad(2), {
                from: user1
            });

        let tx = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        //avoid inconsistance times in differents tests runs
        let result = await superTokenDebug.balanceOf.call(user2);
        let span = result.blocktime - tx.timestamp;
        let finalBalance = span * FLOW_RATE;

        assert.equal(result.balance, finalBalance, "Call: SuperToken.balanceOf - User 2 Super balance incorrect");
    });

    it("Super Balance Additional flow", async() => {

        await web3tx(superToken.upgrade, "Call: SuperToken.update - From user1") (
            toWad(2), {
                from: user1
            });

        let tx = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );

        let tx2 = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user2 -> user3 new Agreement")(
            superToken.address,
            user2,
            user3,
            SECONDARY_FLOW_RATE, {
                from: user2
            }
        );

        await traveler.advanceTime(ADV_TIME * 2);
        await traveler.advanceBlock();

        let result = await superTokenDebug.balanceOf.call(user2);
        let result2 = await superTokenDebug.balanceOf.call(user3);

        //avoid inconsistance times in differents tests runs
        let span = result.blocktime - tx.timestamp;
        let span2 = result2.blocktime - tx2.timestamp;

        let userBalance = FLOW_RATE * span;
        let otherBalance = SECONDARY_FLOW_RATE * span2;
        let user2Balance = userBalance - otherBalance;

        let user3Balance = SECONDARY_FLOW_RATE * span2;

        assert.equal(result.balance, user2Balance, "Call: SuperToken.balanceOf - User 2 Super balance incorrect");
        assert.equal(result2.balance, user3Balance, "Call: SuperToken.balanceOf - User 3 Super balance incorrect");

    });

    it("Should update a existing flow", async () => {

        let tx1 = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        const {timestamp} = await web3.eth.getBlock("latest");
        const addicionalState = web3.eth.abi.encodeParameters(["uint256","int256"], [timestamp, FLOW_RATE_UPDATED.toString()]);

        let tx2 = await web3tx(agreement.updateFlow, "Call: FlowAgreement.updateFlow - user1 -> user2 updating Agreement")(
            superToken.address,
            user1,
            user2,
            addicionalState, {
                from: user1
            }
        );

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        //await new Promise(r => setTimeout(r, 12000));

        let result = await superTokenDebug.balanceOf.call(user2);
        let span_oldBalance = result.blocktime - tx1.timestamp;
        let span_newBalance = result.blocktime - tx2.timestamp;
        let totalBalance = (span_oldBalance * FLOW_RATE) +  (span_newBalance * FLOW_RATE_UPDATED);

        assert.equal(result.balance.toString(), totalBalance.toString(), "Call: SuperToken.balanceOf - Update state - User2 don't add up");
    });

    it("Should delete an existing flow", async () => {

        let tx1 = await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        //Here we have 2 Token in balance, see the last test
        let tx2 = await web3tx(agreement.deleteFlow, "Call: FlowAgreement.deleteFlow - user1 -> user2 Delete an Agreement")(
            superToken.address,
            user1,
            user2, {
                from: user1
            }
        );

        let span = tx2.timestamp - tx1.timestamp;
        console.log("SPAN: ", span);

        let totalBalance = (span * FLOW_RATE);

        await traveler.advanceTime(ADV_TIME * 10000);
        await traveler.advanceBlock();
        let result = await superTokenDebug.balanceOf.call(user2);

        assert.equal(result.balance.toString(), totalBalance.toString(), "Call: SuperToken.balanceOf - Delete state - User2 dont add up");
    });

    it("Create a new flow and Test FlowRate", async () => {

        const finalFlowRate = "-2000000000000000100";

        await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new agreement")(
            superToken.address,
            user1,
            user2,
            100, {
                from: user1
            }
        );

        let stateUser1 = await superToken.getState.call(agreement.address, user1);
        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let flowRate = await agreement.getFlowRate.call(superToken.address, user1, user2);

        assert.equal(flowRate, splitUser1[1], "Call: FlowAgreement.getFlowRate - FlowRate changed");

        const {timestamp} = await web3.eth.getBlock("latest");
        const addicionalState = web3.eth.abi.encodeParameters(["uint256","int256"], [timestamp, "2000000000000000000"]);

        await web3tx(agreement.updateFlow, "Call: FlowAgreement.updateFlow - user1 -> user2 updating Agreement")(
            superToken.address,
            user1,
            user2,
            addicionalState, {
                from: user1
            }
        );

        flowRate = await agreement.getFlowRate.call(superToken.address, user1, user2);
        assert.equal(flowRate, finalFlowRate, "Call: FlowAgreement.getFlowRate - Not getting the correct flow Rate");
    });

    it("Account Balances", async () => {

        await web3tx(agreement.createFlow, "Call: FlowAgreement.createFlow - user1 -> user2 new agreement")(
            superToken.address,
            user1,
            user2,
            100, {
                from: user1
            }
        );

        let user1Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user1);
        let user1Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user1);

        let user2Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user2);
        let user2Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user2);

        //at this stage the user 1 should have a debit of -100 and 0 credits
        assert.equal(user1Debitor, -100, "Call: FlowAgreement.getTotalOutFlowRate -  User 1 Debit account not correct #1");
        assert.equal(user1Creditor, 0, "Call: FlowAgreement.getTotalInFlowRate - User 1 Credit account nor correct #1");

        //at this state user 2 should have a credit of +100 and debit of 0
        assert.equal(user2Debitor, 0, "Call: FlowAgreement.getTotalOutFlowRate - User 2 Debit account not correct #1");
        assert.equal(user2Creditor, 100, "Call: FlowAgreement.getTotalInFlowRate - User 2 Credit account nor correct #1");

        await web3tx(agreement.createFlow, "user2 -> user1 update agreement")(
            superToken.address,
            user2,
            user1,
            1000, {
                from: user2
            }
        );

        user1Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user1);
        user1Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user1);
        user2Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user2);
        user2Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user2);

        //at this stage the user 1 should have a debit of -100 and 1000 credits
        assert.equal(user1Debitor, -100, "Call: FlowAgreement.getTotalOutFlowRate - User 1 Debit account not correct #2");
        assert.equal(user1Creditor, 1000, "Call: FlowAgreement.getTotalInFlowRate - User 1 Credit account nor correct #2");

        //at this state user 2 should have a credit of +100 and debit of -1000
        assert.equal(user2Debitor, -1000, "Call: FlowAgreement.getTotalOutFlowRate - User 2 Debit account not correct #2");
        assert.equal(user2Creditor, 100, "Call: FlowAgreement.getTotalInFlowRate - User 2 Credit account nor correct #2");

        await web3tx(agreement.updateFlow, "Call: FlowAgreement.updateFlow - user2 -> user1 update agreement")(

            superToken.address,
            user2,
            user1,
            -100, {
                from: user2
            }
        );

        user1Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user1);
        user1Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user1);

        user2Debitor = await agreement.getTotalOutFlowRate.call(superToken.address, user2);
        user2Creditor = await agreement.getTotalInFlowRate.call(superToken.address, user2);

        //at this stage the user 1 should have a debit of -100 and 898 credits
        assert.equal(user1Debitor, -100, "Call: FlowAgreement.getTotalOutFlowRate - User 1 Debit account not correct #3");
        assert.equal(user1Creditor, 900, "Call: FlowAgreement.getTotalInFlowRate - User 1 Credit account nor correct #3");

        //at this state user 2 should have a credit of +100 and debit of -898
        assert.equal(user2Debitor, -900, "Call: FlowAgreement.getTotalOutFlowRate - User 2 Debit account not correct #3");
        assert.equal(user2Creditor, 100, "Call: FlowAgreement.getTotalInFlowRate - User 2 Credit account nor correct #3");
    });
});
