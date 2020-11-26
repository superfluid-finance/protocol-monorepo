const { expectRevert } = require("@openzeppelin/test-helpers");

const TestEnvironment = require("../TestEnvironment");
const SuperUpgrader = artifacts.require("SuperUpgrader");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-helpers");

contract("Superfluid Super Upgrader Contract", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 3));
    const { admin, alice, bob } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;

    const backend = bob;

    let superToken;
    let testToken;
    
    before(async () => {
        await t.reset();
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({
            superToken,
            testToken
        } = t.contracts);
    });

    describe("#1 SuperUpgrader Deployement", async () => {

        it("#1.1 Should deploy backend address", async () => {
            const upgrader = await SuperUpgrader.new(backend);
            const contractBackendAddress = await upgrader.owner.call();
            assert.equal(backend,
                contractBackendAddress,
                "SuperUpgrader contract should have backend address as owner");
        });

        it("#1.2 Should revert without owner address", async () => {
            await expectRevert(
                SuperUpgrader.new(ZERO_ADDRESS),
                "Backend Address can't be zero");
        });
    });

    describe("#2 Upgrades to SuperToken", async () => {

        it("#2.1 Should revert if not backend", async () => {
            const upgrader = await SuperUpgrader.new(backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to admin")(
                admin,
                toWad("3"), {
                    from: alice
                }
            );
            await expectRevert(
                upgrader.upgrade(superToken.address, alice, toWad("3"), { from: alice}),
                "Ownable: caller is not the owner");
        });

        it("#2.2 Should upgrade amount and give it back to user", async () => {
            const upgrader = await SuperUpgrader.new(backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to admin")(
                upgrader.address,
                toWad("3"), {
                    from: alice
                }
            );
            await web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                toWad("3"), {
                    from: backend
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(), toWad("3"), "Alice should receive the correct amount");
        });

        it("#2.3 Should upgrade small amount", async() => {
            const upgrader = await SuperUpgrader.new(backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to backend")(
                upgrader.address,
                toWad("3"), {
                    from: alice
                }
            );

            await web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                1, {
                    from: backend
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(), "1", "Alice should receive the correct amount");
        });

        it("#2.4 Should upgrade large amount", async() => {
            const upgrader = await SuperUpgrader.new(backend);
            await testToken.mint(alice, toWad("100000000000"), {
                from: admin
            });

            await web3tx(testToken.approve, "testToken.approve - from alice to backend")(
                upgrader.address,
                toWad("100000000000"), {
                    from: alice
                }
            );

            await web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                toWad("100000000000"), {
                    from: backend
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(),
                toWad("100000000000").toString(),
                "Alice should receive the correct amount");
        });

        it("#2.5 Should revert without approval", async() => {
            const upgrader = await SuperUpgrader.new(backend);

            await expectRevert(web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                1, {
                    from: backend
                }
            ), "ERC20: transfer amount exceeds allowance");
        });

        it("#2.6 Should revert approval is less than need it", async() => {
            const upgrader = await SuperUpgrader.new(backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to backend")(
                upgrader.address,
                toWad("1"), {
                    from: alice
                }
            );

            await expectRevert(web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                "1000000000000000001", {
                    from: backend
                }
            ), "ERC20: transfer amount exceeds allowance");
        });
    });
});
