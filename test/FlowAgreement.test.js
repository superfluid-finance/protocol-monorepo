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
const INI_BALANCE = toWad(1000);

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

        const beginBlock = await web3.eth.getBlock(tx.receipt.blockNumber);
        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let user1Balance = await superToken.balanceOf.call(user1);
        let user2Balance = await superToken.balanceOf.call(user2);
        let span = endBlock.timestamp - beginBlock.timestamp;
        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = (span * FLOW_RATE);

        assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
    });


    it("#1.1 Create Flow One to Many - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let tx2 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user1});
        await traveler.advanceTimeAndBlock(ADV_TIME);

        const block1 = await web3.eth.getBlock(tx.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let user1Balance = await superToken.balanceOf.call(user1);
        let user2Balance = await superToken.balanceOf.call(user2);
        let user3Balance = await superToken.balanceOf.call(user3);
        const endBlock = await web3.eth.getBlock("latest");

        let span2 = endBlock.timestamp - block1.timestamp;
        let span3 = endBlock.timestamp - block2.timestamp;

        let finalUser2 = (span2 * FLOW_RATE);
        let finalUser3 = (span3 * FLOW_RATE);
        let finalUser1 = INI_BALANCE - finalUser2 - finalUser3;

        assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
        assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wring");

    });

    it("#2 Running Flow downgrade small portion of tokens", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let smallPortion = new web3.utils.toBN(1000);
        let userTokenBalance = await token.balanceOf.call(user2);

        await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

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

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let tx2 = await web3tx(
            agreement.deleteFlow,
            "Call: FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
        )(superToken.address, user1, user2, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let user1Balance = await superToken.balanceOf.call(user1);
        let user2Balance = await superToken.balanceOf.call(user2);

        let span = block2.timestamp - block1.timestamp;
        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = (span * FLOW_RATE);

        assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
    });

    it("#4.1 Running multi Flows Stop One Flow - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let txFlow12 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, (FLOW_RATE * 10).toString(), {from: user1});

        let txFlow23 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 3 Create new Flow"
        )(superToken.address, user3, FLOW_RATE, {from: user2});

        let txFlow24 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 2 -> User 4 Create new Flow"
        )(superToken.address, user4, FLOW_RATE, {from: user2});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let txflow12End = await web3tx(
            agreement.deleteFlow,
            "Call: FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
        )(superToken.address, user1, user2, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);
        const endBlock = await web3.eth.getBlock("latest");

        let user1Balance = await superToken.balanceOf.call(user1);
        let user2Balance = await superToken.balanceOf.call(user2);
        let user3Balance = await superToken.balanceOf.call(user3);
        let user4Balance = await superToken.balanceOf.call(user4);

        const blockFlow12 = await web3.eth.getBlock(txFlow12.receipt.blockNumber);
        const blockFlow23 = await web3.eth.getBlock(txFlow23.receipt.blockNumber);
        const blockFlow24 = await web3.eth.getBlock(txFlow24.receipt.blockNumber);
        const blockFlow12End = await web3.eth.getBlock(txflow12End.receipt.blockNumber);
        let spanFlow12 = blockFlow12End.timestamp - blockFlow12.timestamp;
        let spanFlow23 = endBlock.timestamp - blockFlow23.timestamp;
        let spanFlow24 = endBlock.timestamp - blockFlow24.timestamp;

        let finalUser1 = INI_BALANCE - (spanFlow12 * FLOW_RATE * 10);
        let finalUser3 = spanFlow23 * FLOW_RATE;
        let finalUser4 = spanFlow24 * FLOW_RATE;
        let finalUser2 = spanFlow12 * FLOW_RATE * 10 - finalUser3 - finalUser4;

        assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
        assert.equal(user3Balance.toString(), finalUser3.toString(), "User 3 Final balance is wrong");
        assert.equal(user4Balance.toString(), finalUser4.toString(), "User 4 Final balance is wrong");
    });


    it("#4.2 Running Flow Receiver Stop Flow - assert final balance", async() => {

        await superToken.upgrade(INI_BALANCE, {from: user1});

        let tx1 = await web3tx(
            agreement.createFlow,
            "Call: FlowAgreement.createFlow: User 1 -> User 2 Create new Flow"
        )(superToken.address, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let tx2 = await web3tx(
            agreement.deleteFlow,
            "Call: FlowAgreement.deleteFlow: User 1 -> User2 Delete Flow"
        )(superToken.address, user2, user1, {from: user1});

        await traveler.advanceTimeAndBlock(ADV_TIME * 1000);

        let user1Balance = await superToken.balanceOf.call(user1);
        let user2Balance = await superToken.balanceOf.call(user2);

        const block1 = await web3.eth.getBlock(tx1.receipt.blockNumber);
        const block2 = await web3.eth.getBlock(tx2.receipt.blockNumber);
        let span = block2.timestamp - block1.timestamp;
        let finalUser1 = INI_BALANCE - (span * FLOW_RATE);
        let finalUser2 = (span * FLOW_RATE);

        assert.equal(user1Balance.toString(), finalUser1.toString(), "User 1 Final balance is wrong");
        assert.equal(user2Balance.toString(), finalUser2.toString(), "User 2 Final balance is wrong");
    });
});
