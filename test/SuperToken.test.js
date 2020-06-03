const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);
const FLOW_RATE_ADDITIONAL = toWad(2);
const INI_BALANCE = toWad(100);

contract("Super Token", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let token;
    let superToken;
    let agreement;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);
    });

    beforeEach(async () => {

        token = await web3tx(ERC20Mintable.new, "Call: ERC20Mintable.new")(
            {
                from: admin
            });

        await token.mint(user1, INI_BALANCE);
        await token.mint(user2, INI_BALANCE);
        await token.mint(admin, INI_BALANCE);

        superToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")(
            {
                from:admin
            });

        await web3tx(token.approve, "Call: ERC20Mintable.approve - from admin to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: admin
            }
        );

        await web3tx(token.approve, "Call: ERC20Mintable.approve - from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );

        await web3tx(token.approve, "Call: ERC20Mintable.approve - from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );
    });

    /*global afterEach*/
    /*eslint no-undef: "error"*/
    afterEach(async () => {

        let adminTokenBalance = await token.balanceOf.call(admin);
        let adminSuperTokenBalance = await superToken.balanceOf.call(admin);

        let user1TokenBalance = await token.balanceOf.call(user1);
        let user1SuperTokenBalance = await superToken.balanceOf.call(user1);
        let user2TokenBalance = await token.balanceOf.call(user2);
        let user2SuperTokenBalance = await superToken.balanceOf.call(user2);

        console.log("Report: admin (%s) Token Balance: %s",admin, adminTokenBalance);
        console.log("Report: admin (%s) SuperToken Balance: %s",admin, adminSuperTokenBalance);
        console.log("Report: user 1 (%s) Token Balance: %s",user1, user1TokenBalance);
        console.log("Report: user 1 (%s) SuperToken Balance: %s",user1, user1SuperTokenBalance);
        console.log("Report: user 2 (%s) Token Balance: %s",user2, user2TokenBalance);
        console.log("Report: user 2 (%s) SuperToken Balance: %s",user2, user2SuperTokenBalance);

    });

    it("#1 - realtimeBalanceOf current block time equals balaceOf", async() => {

        // TODO some actual balance please

        let balanceOf = await superToken.balanceOf.call(user1);
        const {timestamp} = await web3.eth.getBlock("latest");
        let realBalanceOf = await superToken.realtimeBalanceOf.call(user1, timestamp);

        assert.ok(balanceOf.eq(realBalanceOf), "BalanceOf should be the same as realtimeBalanceOf");
    });

    it("#2 - upgrading to SuperToken", async () => {

        let initialBalance = await token.balanceOf.call(user1);

        await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalance = await token.balanceOf.call(user1);
        let finalSuperTokenBalance = await superToken.balanceOf.call(user1);

        assert.isOk(initialBalance.gt(finalBalance), "Call: ERC20Mintable.balanceOf - is wrong");
        assert.equal(finalSuperTokenBalance.toString(), "2000000000000000000", "Call: SuperToken.balanceOf - is wrong");
    });

    it("#2.1 - downgrading from SuperToken by single account", async() => {

        let initialBalance = await token.balanceOf.call(user1);

        await web3tx(superToken.upgrade, "Call: SuperToken.upgrade - from user1") (
            toWad(2), {
                from: user1
            });


        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from user1") (
            toWad(2), {
                from: user1
            });


        let finalBalance = await token.balanceOf.call(user1);
        let finalSuperTokenBalance = await superToken.balanceOf.call(user1);

        assert.isOk(initialBalance.toString(), finalBalance.toString(), "Call: ERC20Mintable.balanceOf - is wrong");
        assert.equal(finalSuperTokenBalance.toString(), "0", "Call: SuperToken.balanceOf - is wrong");
    });


    it("#2.2 - downgrading from SuperToken by multiple accounts", async () => {

        let initialBalanceUser1 = await token.balanceOf.call(user1);
        let initialSuperBalanceUser1 = await superToken.balanceOf.call(user1);

        await superToken.upgrade(toWad(2), {from: user1});
        await superToken.upgrade(toWad(1), {from: user2});

        let initialSuperBalanceUser2 = await superToken.balanceOf.call(user2);

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalanceUser1 = await token.balanceOf.call(user1);
        let finalSuperBalanceUser1 = await superToken.balanceOf.call(user1);
        let finalSuperBalanceUser2 = await superToken.balanceOf.call(user2);

        assert.equal(
            initialBalanceUser1.toString(),
            finalBalanceUser1.toString(),
            "Call: ERC20Mintable.balanceOf - not correct for user 1");
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

        await superToken.upgrade(INI_BALANCE, {from : user1});  //10 tokens

        await superToken.balanceOf.call(user1);
        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        await superToken.balanceOf.call(user1);

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let result2 = await superToken.balanceOf.call(user2);

        let tx2 = await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
            result2, {
                from: user2
            }
        );

        let finalBalance = INI_BALANCE.add(result2);
        let userTokenBalance = await token.balanceOf.call(user2);

        assert.ok(
            userTokenBalance.eq(finalBalance),
            "Call: ERC20Mintable.balanceOf - User 2 token balance is not correct");


        let slippage = await superToken.balanceOf.call(user2);
        if (slippage > 0) {
            console.warn("Detected blockchain time inconsistancy");
        }

        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let result3 = await superToken.balanceOf.call(user1);
        let result4 = await superToken.balanceOf.call(user2);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span1 = endBlock.timestamp - block1.timestamp;
        let span2 = web3.utils.toBN(endBlock.timestamp - block2.timestamp);

        let final1 = INI_BALANCE.sub(web3.utils.toBN(span1 * FLOW_RATE));
        let final2 = slippage.add(span2.mul(FLOW_RATE));

        assert.equal(
            result3.toString(),
            final1.toString(),
            "Call: SuperToken.balanceOf - not correct for user1");
        assert.equal(
            result4.toString(),
            final2.toString(),
            "Call: SuperToken.balanceOf - not correct for user2");
    });

    it("#2.4 - partial amount downgraded with flows running", async() => {

        await superToken.upgrade(INI_BALANCE, {from : user1});
        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let result1 = await superToken.balanceOf.call(user2);
        let half_balance = web3.utils.toBN(result1 / 1000);
        let tx2 = await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
            half_balance.toString(), {
                from: user2
            }
        );

        let finalBalance = INI_BALANCE.add(half_balance);
        let userTokenBalance = await token.balanceOf.call(user2);

        assert.ok(
            userTokenBalance.eq(finalBalance),
            "Call: ERC20Mintable.balanceOf - User 2 token balance is not correct");

        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let result3 = await superToken.balanceOf.call(user1);
        let result4 = await superToken.balanceOf.call(user2);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span1 = endBlock.timestamp - block1.timestamp;
        let span2 = endBlock.timestamp - block2.timestamp;
        let final1 = INI_BALANCE.sub(web3.utils.toBN(span1 * FLOW_RATE));
        let final2 = (span2 * FLOW_RATE) + (result1 - half_balance);

        assert.equal(
            result3.toString(),
            final1.toString(),
            "Call: SuperToken.balanceOf - not correct for user1");
        assert.equal(
            result4.toString(),
            final2.toString(),
            "Call: SuperToken.balanceOf - not correct for user2");
    });

    it("#3 - Check Agreement Data layout", async() => {

        await superToken.upgrade(INI_BALANCE, {from : user1});  //10 tokens
        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        let ab = web3.utils.soliditySha3(user1, user2);
        let ba = web3.utils.soliditySha3(user2, user1);

        let resultAB = await superToken.getAgreementData.call(agreement.address, ab);
        let resultBA = await superToken.getAgreementData.call(agreement.address, ba);

        let splitAB = web3.eth.abi.decodeParameters(["uint256", "int256"], resultAB);
        let splitBA = web3.eth.abi.decodeParameters(["uint256", "int256"], resultBA);

        const block1 = await web3.eth.getBlock(tx.receipt.blockNumber);

        assert.equal(splitAB[0], block1.timestamp, "User 1 to User 2 wrong timestamp");
        assert.equal(splitAB[1], FLOW_RATE * -1, "User 1 Flow Rate wrong");
        assert.equal(splitBA[0], block1.timestamp, "User 2 to User 1 wrong timestamp");
        assert.equal(splitBA[1], FLOW_RATE, "User 2 Flow Rate wrong");

    });

    it("#3.1 - Check Agreement Data layout - assert that opposite flows cancel each other", async() => {

        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        let ab = web3.utils.soliditySha3(user1, user2);
        let ba = web3.utils.soliditySha3(user2, user1);

        let resultAB = await superToken.getAgreementData.call(agreement.address, ab);
        let resultBA = await superToken.getAgreementData.call(agreement.address, ba);

        let splitAB = web3.eth.abi.decodeParameters(["uint256", "int256"], resultAB);
        let splitBA = web3.eth.abi.decodeParameters(["uint256", "int256"], resultBA);

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        assert.equal(splitAB[0], block.timestamp, "User 1 to User 2 wrong timestamp");
        assert.equal(splitAB[1], FLOW_RATE * -1, "User 1 Flow Rate wrong");
        assert.equal(splitBA[0], block.timestamp, "User 2 to User 1 wrong timestamp");
        assert.equal(splitBA[1], FLOW_RATE, "User 2 Flow Rate wrong");

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user1, FLOW_RATE, {from: user2});

        resultAB = await superToken.getAgreementData.call(agreement.address, ab);
        resultBA = await superToken.getAgreementData.call(agreement.address, ba);

        assert.ok(resultAB == null, "User 1 -> User 2 didn't canceled out flow");
        assert.ok(resultBA == null, "User 2 -> User 1 didn't canceled out flow");

    });

    it("#4 - Check ActiveAgreementClass Register - multiple additions", async() => {

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        let user1AgreementClasses = await superToken.getAccountActiveAgreements.call(user1);
        let user2AgreementClasses = await superToken.getAccountActiveAgreements.call(user2);

        assert.ok(user1AgreementClasses.length == 1, "User 1 number of ActiveAgreementClasses is wrong");
        assert.ok(user2AgreementClasses.length == 1, "User 2 number of ActiveAgreementClasses is wrong");
        assert.equal(user1AgreementClasses[0], agreement.address, "User 1 ActiveAgreementClass is wrong");
        assert.equal(user2AgreementClasses[0], agreement.address, "User 2 ActiveAgreementClass is wrong");

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user1, FLOW_RATE_ADDITIONAL, {from: user2});

        user1AgreementClasses = await superToken.getAccountActiveAgreements.call(user1);
        user2AgreementClasses = await superToken.getAccountActiveAgreements.call(user2);

        assert.ok(user1AgreementClasses.length == 1, "User 1 number of ActiveAgreementClasses is wrong");
        assert.ok(user2AgreementClasses.length == 1, "User 2 number of ActiveAgreementClasses is wrong");
        assert.equal(user1AgreementClasses[0], agreement.address, "User 1 ActiveAgreementClass is wrong");
        assert.equal(user2AgreementClasses[0], agreement.address, "User 2 ActiveAgreementClass is wrong");

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        user1AgreementClasses = await superToken.getAccountActiveAgreements.call(user1);
        user2AgreementClasses = await superToken.getAccountActiveAgreements.call(user2);

        assert.ok(user1AgreementClasses.length == 0, "User 1 number of ActiveAgreementClasses is wrong");
        assert.ok(user2AgreementClasses.length == 0, "User 2 number of ActiveAgreementClasses is wrong");
    });

    it("#5 - Check AgreementState - assert that is saved with the correct data", async() => {

        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        let stateUser1 = await superToken.getAgreementAccountState.call(agreement.address, user1);
        let stateUser2 = await superToken.getAgreementAccountState.call(agreement.address, user2);

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
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        let tx = await web3tx(
            agreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        let stateUser1 = await superToken.getAgreementAccountState.call(agreement.address, user1);
        let stateUser2 = await superToken.getAgreementAccountState.call(agreement.address, user2);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.ok(splitUser1[0] == block.timestamp, "User 1 timestamp in State is wrong");
        assert.equal(splitUser1[1], (-1 * FLOW_RATE) * 2, "User 1 Flow Rate is wrong");

        assert.ok(splitUser2[0] == block.timestamp, "User 2 timestamp in State is wrong");
        assert.equal(splitUser2[1], FLOW_RATE * 2, "User 2 Flow Rate is wrong");
    });

    it("#5.2 - Check AgreementState - assert that State is updated by using the createFlow x2", async() => {

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        const block = await web3.eth.getBlock(tx.receipt.blockNumber);

        let stateUser1 = await superToken.getAgreementAccountState.call(agreement.address, user1);
        let stateUser2 = await superToken.getAgreementAccountState.call(agreement.address, user2);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.ok(splitUser1[0] == block.timestamp, "User 1 timestamp in State is wrong");
        assert.equal(splitUser1[1], (-1 * FLOW_RATE) * 2, "User 1 Flow Rate is wrong");

        assert.ok(splitUser2[0] == block.timestamp, "User 2 timestamp in State is wrong");
        assert.equal(splitUser2[1], FLOW_RATE * 2, "User 2 Flow Rate is wrong");
    });

    it("#6 - Snapshot of Balance - assert passed balance after an update", async() => {

        await superToken.upgrade(INI_BALANCE, {from : user1});
        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow")(superToken.address, user2, FLOW_RATE, {from: user1});

        let snapshot1 = await superToken.getSettledBalance.call(user1);
        let snapshot2 = await superToken.getSettledBalance.call(user2);

        assert.equal(snapshot1, 0, "Call: SuperToken.getSnapshot user 1 is incorrect");
        assert.equal(snapshot2, 0, "Call: SuperToken.getSnapshot user 2 is incorrect");

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let tx2 = await web3tx(
            agreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        snapshot1 = await superToken.getSettledBalance.call(user1);
        snapshot2 = await superToken.getSettledBalance.call(user2);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span1 = block2.timestamp - block1.timestamp;
        let result1 = FLOW_RATE * span1;

        assert.equal(snapshot1, (-1 * snapshot2), "Call: SuperToken.getSnapshot first call user 1 is incorrect");
        assert.equal(snapshot2, result1, "Call: SuperToken.getSnapshot first call user 2 is incorrect");

        await traveler.advanceTimeAndBlock(ADV_TIME);
        let tx3 = await web3tx(
            agreement.deleteFlow,
            "Call FlowAgreement.deleteFlow"
        )(superToken.address, user1, user2, {from: user1});

        let snapshot3 = await superToken.getSettledBalance.call(user1);
        let snapshot4 = await superToken.getSettledBalance.call(user2);

        const block3 = await web3.eth.getBlock(tx3.receipt.blockNumber);
        let span2 = (block3.timestamp - block2.timestamp) + (block3.timestamp - block1.timestamp);
        let result2 = (span2 * FLOW_RATE);

        assert.equal(
            snapshot3.toString(),
            (-1 * snapshot4),
            "Call: SuperToken.getSnapshot second call user 1 is incorrect");
        assert.equal(
            snapshot4.toString(),
            result2.toString(),
            "Call: SuperToken.getSnapshot second call user 2 is incorrect");
    });

    it("#6.1 - Snapshot of Balance - assert that is not time dependancy", async() => {
        await superToken.upgrade(INI_BALANCE, {from : user1});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        await web3tx(
            agreement.updateFlow,
            "Call: FlowAgreement.updateFlow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        let snapshot1 = await superToken.getSettledBalance.call(user2);

        await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

        let snapshot2 = await superToken.getSettledBalance.call(user2);

        assert.equal(snapshot1.toString(), snapshot2.toString(), "Snapshot change with time");


    });
});
