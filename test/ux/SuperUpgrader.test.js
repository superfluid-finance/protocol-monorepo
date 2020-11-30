const { expectRevert } = require("@openzeppelin/test-helpers");

const TestEnvironment = require("../TestEnvironment");
const SuperUpgrader = artifacts.require("SuperUpgrader");

const {
    web3tx,
    toWad
} = require("@decentral.ee/web3-helpers");

const DEFAULT_ADMIN_ROLE = '0x0000000000000000000000000000000000000000000000000000000000000000';
const BACKEND_ROLE= web3.utils.soliditySha3('BACKEND_ROLE');

contract("Superfluid Super Upgrader Contract", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 6));
    const { admin, alice, bob, carol, dan, eve } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;

    const backend = new Array(bob, carol, dan);

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

        it("#1.1 Should deploy adminRole address", async () => {
            console.log(backend);
            const upgrader = await SuperUpgrader.new(admin, backend);
            const isAdminRole = await upgrader.hasRole(DEFAULT_ADMIN_ROLE, admin);
            const isBackend = await upgrader.hasRole(BACKEND_ROLE, backend[0]);
            assert.isOk(isAdminRole,
                "SuperUpgrader contract should have address as admin");
            assert.isOk(isBackend,
                "SuperUpgrader contract should have server as backend");
        });

        it("#1.2 Should revert without owner address", async () => {
            const backendWithZero = [...backend];
            backendWithZero.push(ZERO_ADDRESS);
            await expectRevert(
                SuperUpgrader.new(ZERO_ADDRESS, new Array()),
                "adminRole is empty");

            await expectRevert(
                SuperUpgrader.new(admin, backendWithZero),
                "backend can't be zero");
        });

        it("#1.3 Should add new Backend addresses", async () => {
            const upgrader = await SuperUpgrader.new(admin, new Array());
            await upgrader.grantRole(BACKEND_ROLE, bob);
            const isBackend = await upgrader.hasRole(BACKEND_ROLE, bob);
            assert.isOk(isBackend,
                "SuperUpgrader contract should have server as backend");
        });
    });

    describe("#2 Upgrades to SuperToken", async () => {

        it("#2.1 Should revert if not in role", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to admin")(
                upgrader.address,
                toWad("3"), {
                    from: alice
                }
            );
            await expectRevert(
                upgrader.upgrade(superToken.address, alice, toWad("3"), { from: eve}),
                "operation not allowed");
        });

        it("#2.2 Should upgrade amount and give it back to user", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
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
                    from: backend[0]
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(), toWad("3"), "Alice should receive the correct amount");
        });

        it("#2.3 Should upgrade small amount", async() => {
            const upgrader = await SuperUpgrader.new(admin, backend);
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
                    from: backend[1]
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(), "1", "Alice should receive the correct amount");
        });

        it("#2.4 Should upgrade large amount", async() => {
            const upgrader = await SuperUpgrader.new(admin, backend);
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
                    from: backend[2]
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(),
                toWad("100000000000").toString(),
                "Alice should receive the correct amount");
        });

        it("#2.5 Should revert without approval", async() => {
            const upgrader = await SuperUpgrader.new(admin, backend);

            await expectRevert(web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                1, {
                    from: backend[0]
                }
            ), "ERC20: transfer amount exceeds allowance");
        });

        it("#2.6 Should revert approval is less than need it", async() => {
            const upgrader = await SuperUpgrader.new(admin, backend);
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
                    from: backend[0]
                }
            ), "ERC20: transfer amount exceeds allowance");
        });

        it("#2.7 Owner of tokens can use SuperUpgrader directly", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(testToken.approve, "testToken.approve - from alice to admin")(
                upgrader.address,
                toWad("1000000"), {
                    from: alice
                }
            );
            await web3tx(upgrader.upgrade, "registerAgreementClass typeA")(
                superToken.address,
                alice,
                toWad("3"), {
                    from: alice
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(alice);
            assert.equal(aliceSuperTokenBalance.toString(), toWad("3"), "Alice should receive the correct amount");
        });
    });
});
