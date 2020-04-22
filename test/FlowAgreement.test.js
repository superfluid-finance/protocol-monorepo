const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);
const FLOW_RATE_UPDATED = toWad(2);

contract("Flow Agreement", accounts => {

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

        agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from:admin
            });
        token = await web3tx(ERC20Mintable.new, "ERC20Mintable.new")(
            {
                from: admin
            });

        await token.mint(user1, toWad(10));
        await token.mint(user2, toWad(10));

        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        await web3tx(token.approve, "token.approve from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );
    });

    it("Create a new flow", async () => {

        await web3tx(agreement.createFlow, "User1 -> User2 new Agreement")(
            superToken.address,
            user1,
            user2,
            100, {
                from: user1
            }
        );

        const {timestamp} = await web3.eth.getBlock("latest");

        let stateUser1 = await superToken.getState.call(agreement.address, user1);
        let stateUser2 = await superToken.getState.call(agreement.address, user2);

        let splitUser1 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser1);
        let splitUser2 = web3.eth.abi.decodeParameters(["uint256", "int256"], stateUser2);

        assert.equal(splitUser1[0], timestamp, "User1 start date don't match");
        assert.equal(splitUser1[1], -100, "User 1 Flow Rate incorrect");

        assert.equal(splitUser2[0], timestamp, "User2 start date don't match");
        assert.equal(splitUser2[1], 100, "User2 Flow Rate incorrect");

    });

    it("Super Balance", async() => {

        await web3tx(superToken.upgrade, "SuperToken upgrade from user1") (
            toWad(2), {
                from: user1
            });

        await web3tx(agreement.createFlow, "user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );

        const oldBlockNumber = await web3.eth.getBlockNumber();
        let oldBlock = await web3.eth.getBlock(oldBlockNumber);

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        const currentBlockNumber = await web3.eth.getBlockNumber();
        let block = await web3.eth.getBlock(currentBlockNumber);

        //avoid inconsistance times in differents tests runs
        let adv = block.timestamp - oldBlock.timestamp;

        assert.equal(wad4human(await superToken.balanceOf.call(user2)), wad4human(adv * FLOW_RATE), "Super balance incorrect");
    });

    it("Should update a existing flow", async () => {

        const {timestamp} = await web3.eth.getBlock("latest");

        const addicionalState = web3.eth.abi.encodeParameters(["uint256","int256"], [timestamp, "2000000000000000000"]);

        await web3tx(agreement.createFlow, "user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );


        let oldBlockNumber = await web3.eth.getBlockNumber();
        let oldBlock = await web3.eth.getBlock(oldBlockNumber);

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let currentBlockNumber = await web3.eth.getBlockNumber();
        let block = await web3.eth.getBlock(currentBlockNumber);

        //avoid inconsistance times in differents tests runs
        let adv_oldbalance = block.timestamp - oldBlock.timestamp;

        //Here we have 2 Token in balance, see the last test
        await web3tx(agreement.updateFlow, "user1 -> user2 updating Agreement")(
            superToken.address,
            user1,
            user2,
            addicionalState, {
                from: user1
            }
        );

        oldBlockNumber = await web3.eth.getBlockNumber();
        oldBlock = await web3.eth.getBlock(oldBlockNumber);

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        currentBlockNumber = await web3.eth.getBlockNumber();
        block = await web3.eth.getBlock(currentBlockNumber);

        let adv_newBalance = block.timestamp - oldBlock.timestamp;
        let totalBalance = (adv_oldbalance * FLOW_RATE) + (adv_newBalance * FLOW_RATE_UPDATED) + (adv_newBalance * FLOW_RATE);

        //We update the state to be 2 per second.
        assert.equal(wad4human(await superToken.balanceOf(user2)), wad4human(totalBalance), "Update state - User2 don't add up");
    });


    it("Should delete an existing flow", async () => {


        await web3tx(agreement.createFlow, "user1 -> user2 new Agreement")(
            superToken.address,
            user1,
            user2,
            FLOW_RATE, {
                from: user1
            }
        );


        let oldBlockNumber = await web3.eth.getBlockNumber();
        let oldBlock = await web3.eth.getBlock(oldBlockNumber);

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        let currentBlockNumber = await web3.eth.getBlockNumber();
        let block = await web3.eth.getBlock(currentBlockNumber);

        //avoid inconsistance times in differents tests runs
        let adv_oldbalance = block.timestamp - oldBlock.timestamp;

        //Here we have 2 Token in balance, see the last test
        await web3tx(agreement.deleteFlow, "user1 -> user2 Delete an Agreement")(
            superToken.address,
            user1,
            user2, {
                from: user1
            }
        );

        oldBlockNumber = await web3.eth.getBlockNumber();
        oldBlock = await web3.eth.getBlock(oldBlockNumber);

        await traveler.advanceTime(ADV_TIME);
        await traveler.advanceBlock();

        currentBlockNumber = await web3.eth.getBlockNumber();
        block = await web3.eth.getBlock(currentBlockNumber);

        let totalBalance = (adv_oldbalance * FLOW_RATE);
        await traveler.advanceTime(ADV_TIME * 10000);
        await traveler.advanceBlock();

        //We update the state to be 2 per second.
        assert.equal(wad4human(await superToken.balanceOf(user2)), wad4human(totalBalance), "Delete state - User2 don't add up");
    });
});
