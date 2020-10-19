const {
    toWad,
} = require("@decentral.ee/web3-helpers");

const {
    shouldBehaveLikeERC20,
} = require("./ERC20.behavior");

const Tester = require("./Tester");

contract("SuperToken's ERC20 compliance", accounts => {

    const tester = new Tester(accounts.slice(0, 4));
    const { alice, bob, carol } = tester.aliases;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        let token;
        let superToken;
        await tester.resetContracts();
        ({
            token,
            superToken,
        } = tester.contracts);
        this.token = superToken;

        await token.approve(superToken.address, toWad(1), { from: alice });
        await superToken.upgrade(toWad(1), { from: alice });
    });

    shouldBehaveLikeERC20("SuperToken", toWad(1), alice, bob, carol);

});
