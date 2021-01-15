const { expectRevert } = require("@openzeppelin/test-helpers");

const UUPSProxiable = artifacts.require("UUPSProxiable");
const TestToken = artifacts.require("TestToken");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");
const SuperTokenFactoryMock = artifacts.require("SuperTokenFactoryMock");
const SuperTokenMock = artifacts.require("SuperTokenMock");

const TestEnvironment = require("../../TestEnvironment");

const { web3tx } = require("@decentral.ee/web3-helpers");

contract("SuperTokenFactory Contract", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 1), {
        isTruffle: true,
        useMocks: true
    });
    //const { admin, alice, bob } = t.aliases;
    const { ZERO_ADDRESS } = t.constants;

    describe("#1 upgradability", () => {
        let superfluid;
        let factory;

        before(async () => {
            await t.reset();
            ({ superfluid } = t.contracts);
            factory = await SuperTokenFactoryMock.at(
                await superfluid.getSuperTokenFactory.call()
            );
        });

        it("#1.1 storage layout", async () => {
            await factory.validateStorageLayout.call();
        });

        it("#1.2 proxiable info", async () => {
            const proxiable = await UUPSProxiable.at(factory.address);
            assert.equal(
                await proxiable.proxiableUUID.call(),
                web3.utils.sha3(
                    "org.superfluid-finance.contracts.SuperTokenFactory.implementation"
                )
            );
        });

        it("#1.3 only host can update the code", async () => {
            assert.equal(await factory.getHost.call(), superfluid.address);
            await expectRevert(
                factory.updateCode(ZERO_ADDRESS),
                "only host can update code"
            );
        });

        it("#1.4 only can initialize once", async () => {
            await expectRevert(
                factory.initialize(),
                "Initializable: contract is already initialized"
            );
        });
    });

    describe("#2 createERC20Wrapper", () => {
        let superfluid;
        let factory;
        let governance;
        let token1;

        beforeEach(async () => {
            ({ superfluid, governance } = t.contracts);
            factory = await SuperTokenFactoryMock.at(
                await superfluid.getSuperTokenFactory.call()
            );
            token1 = await web3tx(TestToken.new, "TestToken.new 1")(
                "Test Token 1",
                "TT1",
                18
            );
        });

        afterEach(async () => {
            // cleanup test environment for other tests
            await t.reset();
        });

        context("#2.a Mock factory", () => {
            async function updateSuperTokenFactory() {
                const SuperTokenFactory42Mock = artifacts.require(
                    "SuperTokenFactory42Mock"
                );
                const factory2Logic = await SuperTokenFactory42Mock.new(
                    superfluid.address
                );
                await web3tx(
                    governance.updateContracts,
                    "governance.updateContracts"
                )(superfluid.address, ZERO_ADDRESS, [], factory2Logic.address);
                await web3tx(
                    await superfluid.getSuperTokenFactoryLogic.call(),
                    factory2Logic.address
                );
            }

            it("#2.a.1 non upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 0
                });
                superToken1 = await SuperTokenMock.at(superToken1.address);
                await updateSuperTokenFactory();
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
                await expectRevert(
                    governance.updateSuperTokenLogic(
                        superfluid.address,
                        superToken1.address
                    ),
                    "UUPSProxiable: not upgradable"
                );
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
            });

            it("#2.a.2 semi upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1
                });
                superToken1 = await SuperTokenMock.at(superToken1.address);
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
                await updateSuperTokenFactory();
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
                await web3tx(
                    governance.updateSuperTokenLogic,
                    "governance.updateSuperTokenLogic"
                )(superfluid.address, superToken1.address);
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "42"
                );
            });

            it("#2.a.3 full upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2
                });
                superToken1 = await SuperTokenMock.at(superToken1.address);
                await updateSuperTokenFactory();
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "42"
                );
                await expectRevert(
                    governance.updateSuperTokenLogic(
                        superfluid.address,
                        superToken1.address
                    ),
                    "UUPSProxiable: not upgradable"
                );
            });
        });

        context("#2.b Production Factory", () => {
            it("#2.b.1 use production factory to create different super tokens", async () => {
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address
                );
                await web3tx(
                    governance.updateContracts,
                    "governance.updateContracts"
                )(superfluid.address, ZERO_ADDRESS, [], factory2Logic.address);
                let superToken0 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 0
                });
                assert.equal(
                    await superToken0.getUnderlyingToken.call(),
                    token1.address
                );
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1
                });
                assert.equal(
                    await superToken1.getUnderlyingToken.call(),
                    token1.address
                );
                let superToken2 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2
                });
                assert.equal(
                    await superToken2.getUnderlyingToken.call(),
                    token1.address
                );
            });
        });

        it("#2.c.1 should fail on ZERO_ADDRESS", async () => {
            await expectRevert(
                factory.createERC20Wrapper(
                    ZERO_ADDRESS,
                    18,
                    0,
                    "name",
                    "symbol"
                ),
                "SuperTokenFactory: zero address"
            );
        });
    });
});
