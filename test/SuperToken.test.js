const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");
const InstruSuperToken = artifacts.require("InstruSuperToken");
const InstruFlowAgreement = artifacts.require("InstruFlowAgreement");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);
const INI_BALANCE = toWad(10);

contract("Super Token", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let token;
    let superToken;
    let agreement;
    let superTokenDebug;
    let agreementDebug;

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

        superTokenDebug = await web3tx(InstruSuperToken.new, "Call InstruSuperToken.new")(
            superToken.address
        );

        agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")(
            {
                from:admin
            });

        agreementDebug = await web3tx(InstruFlowAgreement.new, "Call: InstruFlowAgreement.new")(
            agreement.address, {
                from:admin
            }
        );

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

    it("Upgrade ERC20 Token", async () => {

        let initialBalance = await token.balanceOf.call(user1);

        await web3tx(superToken.upgrade, "Call. SuperToken.upgrade - from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalance = await token.balanceOf.call(user1);

        assert.isOk(initialBalance.gt(finalBalance), "Call: ERC20Mintable.balanceOf - is wrong");

        assert.equal(wad4human(await superToken.balanceOf.call(user1)), "2.00000", "Call: SuperToken.balanceOf -  balance is wrong");

    });

    it("Downgrade ERC20 Token", async () => {

        let initialBalance = await token.balanceOf.call(user1);
        let initialSuperBalance = await superToken.balanceOf.call(user1);

        await superToken.upgrade(toWad(2), {from: user1});

        await web3tx(superToken.downgrade, "Call. SuperToken.downgrade - from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalance = await token.balanceOf.call(user1);
        let finalSuperBalance = await superToken.balanceOf.call(user1);

        assert.equal(wad4human(initialBalance), wad4human(finalBalance), "Call: ERC20Mintable.balanceOf - not correct");
        assert.equal(wad4human(initialSuperBalance), wad4human(finalSuperBalance), "Call: SuperToken.balanceOf - not correct");

    });

    it("Downgrade ERC20 Token with other users", async () => {

        let initialBalanceUser1 = await token.balanceOf.call(user1);
        let initialSuperBalanceUser1 = await superToken.balanceOf.call(user1);


        await superToken.upgrade(toWad(2), {from: user1});
        await superToken.upgrade(toWad(1), {from: user2});

        let initialSuperBalanceUser2 = await superToken.balanceOf.call(user2);

        await web3tx(superToken.downgrade, "Call. SuperToken.downgrade - from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalanceUser1 = await token.balanceOf.call(user1);
        let finalSuperBalanceUser1 = await superToken.balanceOf.call(user1);

        let finalSuperBalanceUser2 = await superToken.balanceOf.call(user2);

        assert.equal(wad4human(initialBalanceUser1), wad4human(finalBalanceUser1), "Call: ERC20Mintable.balanceOf - not correct for user 1");
        assert.equal(wad4human(initialSuperBalanceUser1), wad4human(finalSuperBalanceUser1), "Call: SuperToken.balanceOf - not correct for user 1");
        assert.equal(wad4human(initialSuperBalanceUser2), wad4human(finalSuperBalanceUser2), "Call: SuperToken.balanceOf - not correct for user 2");
    });

    it("Downgrade token in running flows", async() => {

        await superToken.upgrade(INI_BALANCE, {from : user1});

        await agreementDebug.createFlow(superToken.address, user1, user2, FLOW_RATE, {from: user1});

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let result1 = await superTokenDebug.balanceOf.call(user2);

        let tx2 = await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - user 2")(
            result1.balance, {
                from: user2
            }
        );

        await superTokenDebug.balanceOf.call(user2);

        let finalBalance = INI_BALANCE.add(result1.balance);
        let userTokenBalance = await token.balanceOf.call(user2);

        //console.log("User token balance: ", userTokenBalance.toString());
        //console.log("User Final balance: ", finalBalance.toString());
        assert.ok(userTokenBalance.eq(finalBalance), "User 2 token balance is not correct");

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let result3 = await superTokenDebug.balanceOf.call(user2);

        let span = result3.blocktime - tx2.timestamp;
        let final = span * FLOW_RATE;
        assert.equal(result3.balance, final, "Call: SuperToken.balanceOf not correct for user1");
    });
});
