const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

contract("Super Token", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let token;
    let superToken;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);
    });

    beforeEach(async () => {
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


        await web3tx(token.approve, "token.approve from user2 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user2
            }
        );
    });

    it("Upgrade ERC20 Token", async () => {

        let initialBalance = await token.balanceOf.call(user1);

        await web3tx(superToken.upgrade, "SuperToken upgrade from user1") (
            toWad(2), {
                from: user1
            });

        let finalBalance = await token.balanceOf.call(user1);

        assert.isOk(initialBalance.gt(finalBalance), "Token balance is wrong");

        assert.equal(wad4human(await superToken.balanceOf.call(user1)), "2.00000", "SuperToken balance is wrong");

    });
});
