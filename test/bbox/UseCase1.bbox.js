const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    //wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

contract("Flow Agreement Stories", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];
    const user3 = accounts[3];

    let token;
    let superToken;
    let agreement;

    beforeEach(async () => {

        agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from:admin
            });

        token = await web3tx(ERC20Mintable.new, "ERC20Mintable.new")(
            {
                from: admin
            });

        await token.mint(user1, toWad(1000));
        await token.mint(user2, toWad(1000));

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

        await web3tx(token.approve, "token.approve from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );
        await web3tx(superToken.addAgreement, "SuperToken.addAgreement")(
            agreement.address, {
                from: admin
            }
        );
    });

    it("#1 - One user is receiving many flows", async () => {

        await superToken.upgrade(toWad(10), {from: user1});
        await superToken.upgrade(toWad(5), {from: user2});

        const flowRate1 = toWad(10 / 30 / 24 /3600);
        const flowRate2 = toWad(5 / 30 / 24 / 3600);


        await web3tx(agreement.createFlow, "User1 -> User3 new Agreement")(
            superToken.address,
            user3,
            flowRate1, {
                from: user1
            }
        );

        await web3tx(agreement.createFlow, "User2 -> User3 new Agreement")(
            superToken.address,
            user3,
            flowRate2, {
                from: user2
            }
        );


        await traveler.advanceTime(3600 * 24 * 7);
        await traveler.advanceBlock();

        let balance = await superToken.balanceOf.call(user3);

        console.log("HERE ---- > User 3 balance: ", balance.toString());
        await web3tx(superToken.downgrade, "User 3 is downgrading tokens")(
            balance, {
                from: user3
            }
        );

        let finalbalance = await token.balanceOf.call(user3);
        assert.equal(balance.toString(), finalbalance.toString(), "User 3 final balance is not the flow balance");
        console.log("User 3 Downgraded token balance: ", finalbalance.toString());
    });
});
