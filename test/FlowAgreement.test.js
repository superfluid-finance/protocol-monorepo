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
const FLOW_RATE = toWad(1);
const INI_BALANCE = toWad(10);

contract("Flow Agreement", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];
    const user3 = accounts[3];
    const user4 = accounts[4];
    const user5 = accounts[5];
    const user6 = accounts[6];

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

        await token.mint(admin, INI_BALANCE);
        await token.mint(user1, INI_BALANCE);
        await token.mint(user2, INI_BALANCE);
        await token.mint(user3, INI_BALANCE);
        await token.mint(user4, INI_BALANCE);
        await token.mint(user5, INI_BALANCE);
        await token.mint(user6, INI_BALANCE);

        superToken = await web3tx(SuperToken.new, "Call: SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        superTokenDebug = await web3tx(InstruSuperToken.new, "Call: InstruSuperToken.new")(
            superToken.address
        );

        await web3tx(token.approve, "Call: token.approve from admin to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: admin
            }
        );

        await web3tx(token.approve, "Call: token.approve from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );

        await web3tx(token.approve, "Call: token.approve from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );

        await web3tx(token.approve, "Call: token.approve from user3 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user3
            }
        );

        await web3tx(token.approve, "Call: token.approve from user4 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user4
            }
        );

        await web3tx(token.approve, "Call: token.approve from user5 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user5
            }
        );

        await web3tx(token.approve, "Call: token.approve from user6 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user6
            }
        );

    });

    it("#1 Create Flow One to One - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let user1Balance = await superTokenDebug.balanceOf.call(user1);
        let user2Balance = await superTokenDebug.balanceOf.call(user2);
        let span = user1Balance.blocktime - tx.timestamp;
        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = (span * FLOW_RATE);

        assert.equal(user1Balance.balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
    });


    it("#1.1 Create Flow One to Many - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let tx2 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user1});
        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let user1Balance = await superTokenDebug.balanceOf.call(user1);
        let user2Balance = await superTokenDebug.balanceOf.call(user2);
        let user3Balance = await superTokenDebug.balanceOf.call(user3);

        let span2 = user2Balance.blocktime - tx.timestamp;
        let span3 = user3Balance.blocktime - tx2.timestamp;

        let finalUser2 = (span2 * FLOW_RATE);
        let finalUser3 = (span3 * FLOW_RATE);
        let finalUser1 = INI_BALANCE - finalUser2 - finalUser3;

        assert.equal(user1Balance.balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
        assert.equal(user3Balance.balance.toString(), finalUser3.toString(), "User 3 Final balance is wring");

    });

    it("#2 Running Flow downgrade small portion of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let smallPortion = new web3.utils.toBN(1000);
        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade: User 2 Downgrade")(smallPortion, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(smallPortion).toString(),
            "User2 downgrade call dont change the token balance");
    });

    it("#2.1 Running Flow downgrade 1/2 portion of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let halfPortion= new web3.utils.toBN(500000000000000000);
        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        await web3tx(superToken.downgrade, "Call: SuperToken. owngrade: User 2 Downgrade")(halfPortion, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(halfPortion).toString(),
            "User2 downgrade call dont change the token balance");

    });

    it("#2.2 Running Flow downgrade total balance of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let userTokenBalance = await token.balanceOf.call(user2);
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let userSuperBalance = await superToken.balanceOf.call(user2);
        await web3tx(
            superToken.downgrade,
            "Call: SuperToken.downgrade: User 2 Downgrade"
        )(userSuperBalance, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(userSuperBalance).toString(),
            "User2 downgrade call dont change the token balance");
    });

    it("#3 Running Multi Flow downgrade small portion of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});
        await superToken.upgrade(INI_BALANCE, {from: user2});

        let smallPortion = new web3.utils.toBN(1000);
        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 3 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user2});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 3 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user3});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade: User 2 Downgrade")(smallPortion, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(smallPortion).toString(),
            "User2 downgrade call dont change the token balance");
    });

    it("#3.1 Running Multi Flow downgrade 1/2 portion of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});
        await superToken.upgrade(INI_BALANCE, {from: user2});

        let halfPortion= new web3.utils.toBN(1000000000000000000);
        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 3 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user2});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 3 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user3});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade: User 2 Downgrade")(halfPortion, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(halfPortion).toString(),
            "User2 downgrade call dont change the token balance");
    });

    it("#3.2 Running Multi Flow downgrade total balance of tokens", async() => {
        await superToken.upgrade(INI_BALANCE, {from: user1});
        await superToken.upgrade(INI_BALANCE, {from: user2});

        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 3 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user2});
        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 3 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user3});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let userSuperBalance = await superToken.balanceOf.call(user2);
        await web3tx(
            superToken.downgrade,
            "Call: SuperToken.downgrade: User 2 Downgrade"
        )(userSuperBalance, {from: user2});
        let userTokenBalanceFinal = await token.balanceOf.call(user2);

        assert.equal(
            userTokenBalanceFinal.toString(),
            userTokenBalance.add(userSuperBalance).toString(),
            "User2 downgrade call dont change the token balance");
    });

    it("#4 Running Flow Stop Flow - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let tx2 = await web3tx(
            agreement.deleteFlow,
            "Call: FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
        )(superToken.address, user2, {from: user1});

        await traveler.advanceTime(ADV_TIME * 1000);
        await traveler.advanceBlock();

        let user1Balance = await superTokenDebug.balanceOf.call(user1);
        let user2Balance = await superTokenDebug.balanceOf.call(user2);

        let span = tx2.timestamp - tx1.timestamp;
        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = (span * FLOW_RATE);

        assert.equal(user1Balance.balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
    });

    it("#4.1 Running multi Flows Stop One Flow - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});
        let tx2 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 3 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user2});
        let tx3 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 4 Create new Flow"
        )(superToken.address, user4, FLOW_RATE, {from: user2});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let tx4 = await web3tx(
            agreement.deleteFlow,
            "Call: FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
        )(superToken.address, user2, {from: user1});

        await traveler.advanceTime(ADV_TIME * 10);
        await traveler.advanceBlock();

        let user1Balance = await superTokenDebug.balanceOf.call(user1);
        let user2Balance = await superTokenDebug.balanceOf.call(user2);
        let user3Balance = await superTokenDebug.balanceOf.call(user3);
        let user4Balance = await superTokenDebug.balanceOf.call(user3);

        let span = tx4.timestamp - tx1.timestamp;
        let span3 = user3Balance.blocktime - tx2.timestamp;
        let span4 = user4Balance.blocktime - tx3.timestamp;

        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = 0;
        let finalUser3 = span3 * FLOW_RATE;
        let finalUser4 = span4 * FLOW_RATE;

        assert.equal(user1Balance.balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
        assert.equal(user3Balance.balance.toString(), finalUser3.toString(), "User 3 Final balance is wrong");
        assert.equal(user4Balance.balance.toString(), finalUser4.toString(), "User 4 Final balance is wrong");
    });

});
