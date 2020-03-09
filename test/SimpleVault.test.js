const ERC20Mintable = artifacts.require("ERC20Mintable");
const SimpleVault = artifacts.require("SimpleVault");
const {
    expectRevert
} = require("@openzeppelin/test-helpers");
const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

contract("SimpleVault", accounts => {

    const admin = accounts[0];
    const user = accounts[1];

    let token;
    let vault;

    before(async () => {
        console.log("admin is", admin);
        console.log("user is", user);
    });

    beforeEach(async () => {
        token = await web3tx(ERC20Mintable.new, "ERC20Mintable.new")(
            {
                from: admin
            });
        vault = await web3tx(SimpleVault.new, "SimpleVault.new")(
            token.address,
            {
                from: admin
            }
        );
        await web3tx(token.mint, "token.mint 1000 -> admin")(
            admin, toWad(1000), {
                from: admin
            });
        await web3tx(token.transfer, "token transfer admin -> user 1000")(
            user, toWad(1000), {
                from: admin
            }
        );
    });

    it("basic operations", async () => {
        assert.equal(wad4human(await vault.balances.call(user)), "0.00000");
        await web3tx(token.approve, "approving vault uses 200 tokens from user")(
            vault.address,
            toWad(200),
            {
                from: user
            }
        );
        await expectRevert(
            vault.deposit(toWad(200.1)),
            "ERC20: transfer amount exceeds balance.");
        await web3tx(vault.deposit, "deposit 200 tokens to vault")(
            toWad(200),
            {
                from: user
            }
        );
        assert.equal(wad4human(await token.balanceOf.call(vault.address)), "200.00000");
        assert.equal(wad4human(await vault.balances.call(user)), "200.00000");
    });

});
