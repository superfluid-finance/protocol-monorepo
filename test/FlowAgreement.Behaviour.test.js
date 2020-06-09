const FlowAgreement = artifacts.require("FlowAgreement");
const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-test-helpers");


const traveler = require("ganache-time-traveler");

const ADV_TIME = 2;
const FLOW_RATE = toWad(1);
const INI_BALANCE = toWad(10);

contract("FlowAgreement Behaviour", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let token;
    let agreement;
    let superToken;

    beforeEach(async () => {

        token = await web3tx(TestToken.new, "TestToken.new")(
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

        agreement = await web3tx(FlowAgreement.new, "Call: FlowAgreement.new")(
            {
                from:admin
            });


        await web3tx(token.approve, "Call: TestToken.approve - from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );


        await web3tx(token.approve, "Call: ERC20Mintabl.approve - from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );

    });

    it("#1 - Should not close other users flow assert revert message", async () => {

        await superToken.upgrade(INI_BALANCE, {from : user1});
        await agreement.createFlow(superToken.address, user2, FLOW_RATE);

        await traveler.advanceTimeAndBlock(ADV_TIME);

        let emitError = false;
        try {
            await web3tx(
                agreement.deleteFlow,
                "Call: FlowAgreement.deleteFlow - Invoking method from another account")(
                superToken.address,
                user1,
                user2, {
                    from: admin
                });

        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "Not the sender or receiver");
        }

        if(!emitError) {
            throw ("Call: FlowAgreement.deleteFlow - error not emitted");
        }
    });

    it("#2 - Attack Flow Creation - Create a negative Flow - Steal User 2", async() => {

        await superToken.upgrade(INI_BALANCE, {from : user1});
        let emitError = false;

        try {
            await agreement.createFlow(superToken.address, user2, "-10000000000000000000000", {from: user1});

        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "Flipping Flow");
        }

        if(!emitError) {
            throw ("Call: FlowAgreement.createFlow - error not emitted");
        }

    });

    it("#2.1 - Attack Flow Update - Update a to get a negative Flow - Steal User 2", async() => {

        await superToken.upgrade(toWad(1), {from : user1});
        let emitError = false;

        try {
            await agreement.createFlow(superToken.address, user2, toWad(1), {from: user1});
            await traveler.advanceTimeAndBlock(ADV_TIME);
            await agreement.createFlow(superToken.address, user2, "-10000000000000000000000", {from: user1});

        } catch(err) {
            emitError = true;
            console.log(err.reason);
            assert.strictEqual(err.reason, "Flipping Flow");

        }

        if(!emitError) {
            throw ("Call: FlowAgreement.createFlow - error not emitted");
        }

    });
});
