const {expectRevert} = require("@openzeppelin/test-helpers");

const TestEnvironment = require("../../TestEnvironment");
const SuperUpgrader = artifacts.require("SuperUpgrader");

const {web3tx, toWad} = require("@decentral.ee/web3-helpers");

const DEFAULT_ADMIN_ROLE =
    "0x0000000000000000000000000000000000000000000000000000000000000000";
const BACKEND_ROLE = web3.utils.soliditySha3("BACKEND_ROLE");

describe("Superfluid Super Upgrader Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;

    let admin, alice, bob, carol, dan, eve;
    let backend;
    let superToken;
    let testToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 6,
        });

        ({admin, alice, bob, carol, dan, eve} = t.aliases);
        backend = new Array(bob, carol, dan);
        testToken = await t.sf.contracts.TestToken.at(t.sf.tokens.TEST.address);
        superToken = t.sf.tokens.TESTx;
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    describe("#1 SuperUpgrader Deployement", async () => {
        it("#1.1 Should deploy adminRole address", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            const isAdminRole = await upgrader.hasRole(
                DEFAULT_ADMIN_ROLE,
                admin
            );
            const isBackend = await upgrader.hasRole(BACKEND_ROLE, backend[0]);
            assert.isOk(
                isAdminRole,
                "SuperUpgrader contract should have address as admin"
            );
            assert.isOk(
                isBackend,
                "SuperUpgrader contract should have server as backend"
            );
        });

        it("#1.2 Should revert without owner address", async () => {
            const backendWithZero = [...backend];
            backendWithZero.push(ZERO_ADDRESS);
            await expectRevert(
                SuperUpgrader.new(ZERO_ADDRESS, new Array()),
                "adminRole is empty"
            );

            await expectRevert(
                SuperUpgrader.new(admin, backendWithZero),
                "backend can't be zero"
            );
        });

        it("#1.3 Should add new Backend addresses", async () => {
            const upgrader = await SuperUpgrader.new(admin, new Array());
            await upgrader.grantRole(BACKEND_ROLE, bob);
            const isBackend = await upgrader.hasRole(BACKEND_ROLE, bob);
            assert.isOk(
                isBackend,
                "SuperUpgrader contract should have server as backend"
            );
        });
    });

    describe("#2 Upgrades to SuperToken", async () => {
        it("#2.1 Should revert if not in role", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to admin"
            )(upgrader.address, toWad("3"), {
                from: alice,
            });
            await expectRevert(
                upgrader.upgrade(superToken.address, alice, toWad("3"), {
                    from: eve,
                }),
                "operation not allowed"
            );
        });

        it("#2.2 Should upgrade amount and give it back to user", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to admin"
            )(upgrader.address, toWad("3"), {
                from: alice,
            });
            await web3tx(upgrader.upgrade, "upgrader.upgrade")(
                superToken.address,
                alice,
                toWad("3"),
                {
                    from: backend[0],
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            assert.equal(
                aliceSuperTokenBalance.toString(),
                toWad("3"),
                "Alice should receive the correct amount"
            );
        });

        it("#2.3 Should upgrade small amount", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to backend"
            )(upgrader.address, toWad("3"), {
                from: alice,
            });

            await web3tx(upgrader.upgrade, "upgrader.upgrade")(
                superToken.address,
                alice,
                1,
                {
                    from: backend[1],
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            assert.equal(
                aliceSuperTokenBalance.toString(),
                "1",
                "Alice should receive the correct amount"
            );
        });

        it("#2.4 Should upgrade large amount", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await testToken.mint(alice, toWad("100000000000"), {
                from: admin,
            });

            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to backend"
            )(upgrader.address, toWad("100000000000"), {
                from: alice,
            });

            await web3tx(upgrader.upgrade, "upgrader.upgrade")(
                superToken.address,
                alice,
                toWad("100000000000"),
                {
                    from: backend[2],
                }
            );

            const aliceSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            assert.equal(
                aliceSuperTokenBalance.toString(),
                toWad("100000000000").toString(),
                "Alice should receive the correct amount"
            );
        });

        it("#2.5 Should revert without approval", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);

            await expectRevert(
                web3tx(upgrader.upgrade, "upgrader.upgrade")(
                    superToken.address,
                    alice,
                    1,
                    {
                        from: backend[0],
                    }
                ),
                "ERC20: insufficient allowance"
            );
        });

        it("#2.6 Should revert approval is less than need it", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to backend"
            )(upgrader.address, toWad("1"), {
                from: alice,
            });

            await expectRevert(
                web3tx(upgrader.upgrade, "upgrader.upgrade")(
                    superToken.address,
                    alice,
                    "1000000000000000001",
                    {
                        from: backend[0],
                    }
                ),
                "ERC20: insufficient allowance"
            );
        });

        it("#2.7 Owner of tokens can use SuperUpgrader directly", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to admin"
            )(upgrader.address, toWad("1000000"), {
                from: alice,
            });
            await web3tx(upgrader.upgrade, "upgrader.upgrade")(
                superToken.address,
                alice,
                toWad("3"),
                {
                    from: alice,
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            assert.equal(
                aliceSuperTokenBalance.toString(),
                toWad("3"),
                "Alice should receive the correct amount"
            );
        });

        it("#2.8 Owner should define optout/optin blocking backend upgrade", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(
                testToken.approve,
                "testToken.approve - from alice to backend"
            )(upgrader.address, toWad("1000000"), {
                from: alice,
            });

            await web3tx(
                upgrader.optoutAutoUpgrades,
                "Alice opt-out"
            )({from: alice});

            await expectRevert(
                upgrader.upgrade(superToken.address, alice, toWad("3"), {
                    from: backend[0],
                }),
                "operation not allowed"
            );

            await web3tx(
                upgrader.optinAutoUpgrades,
                "Alice opt-in"
            )({from: alice});

            await web3tx(upgrader.upgrade, "Backend upgrade alice tokens")(
                superToken.address,
                alice,
                toWad("100"),
                {
                    from: backend[0],
                }
            );
            const aliceSuperTokenBalance = await superToken.balanceOf.call(
                alice
            );
            assert.equal(
                aliceSuperTokenBalance.toString(),
                toWad("100"),
                "Alice should receive the correct amount"
            );
        });
    });

    describe("#3 Control list of roles", async () => {
        it("#3.1 Admin should add/remove backend accounts", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);

            for (let i = 0; i < backend.length; i++) {
                assert.isOk(
                    await upgrader.isBackendAgent(backend[i]),
                    "address should be in backend role"
                );
            }

            await web3tx(
                upgrader.revokeBackendAgent,
                "admin revoke backend account"
            )(backend[0], {
                from: admin,
            });

            assert.isOk(
                !(await upgrader.isBackendAgent(backend[0])),
                "address should not be in backend role"
            );

            await web3tx(
                upgrader.grantBackendAgent,
                "admin grant backend account"
            )(backend[0], {
                from: admin,
            });

            assert.isOk(
                await upgrader.isBackendAgent(backend[0]),
                "address should be in backend role"
            );

            await expectRevert(
                upgrader.grantBackendAgent(ZERO_ADDRESS, {from: admin}),
                "operation not allowed"
            );

            await expectRevert(
                upgrader.grantBackendAgent(eve, {from: eve}),
                `AccessControl: account ${eve.toLowerCase()} is missing role ${DEFAULT_ADMIN_ROLE}`
            );

            await expectRevert(
                upgrader.revokeBackendAgent(backend[1], {from: eve}),
                `AccessControl: account ${eve.toLowerCase()} is missing role ${DEFAULT_ADMIN_ROLE}`
            );
        });

        it("#3.2 Admin should add/remove admin accounts", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            await web3tx(upgrader.grantRole, "admin add bob to admin")(
                DEFAULT_ADMIN_ROLE,
                bob,
                {
                    from: admin,
                }
            );
            assert.isOk(
                await upgrader.hasRole(DEFAULT_ADMIN_ROLE, bob),
                "bob should be in admin role"
            );
        });

        it("#3.3 List all Backend accounts", async () => {
            const upgrader = await SuperUpgrader.new(admin, backend);
            const registerBackend = await upgrader.getBackendAgents.call();
            for (let i = 0; i < backend.length; i++) {
                assert.equal(
                    registerBackend[i],
                    backend[i],
                    "not register backend"
                );
            }
        });
    });
});
