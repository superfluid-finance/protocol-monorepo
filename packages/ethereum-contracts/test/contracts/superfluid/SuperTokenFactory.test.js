const {expectEvent} = require("@openzeppelin/test-helpers");
const {expectRevertedWith} = require("../../utils/expectRevert");

const UUPSProxiable = artifacts.require("UUPSProxiable");
const TestToken = artifacts.require("TestToken");
const SuperTokenFactoryHelper = artifacts.require("SuperTokenFactoryHelper");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");
const SuperTokenFactoryMockHelper = artifacts.require(
    "SuperTokenFactoryMockHelper"
);
const FullUpgradableSuperTokenProxy = artifacts.require(
    "FullUpgradableSuperTokenProxy"
);
const SuperTokenMock = artifacts.require("SuperTokenMock");

const TestEnvironment = require("../../TestEnvironment");

const {web3tx} = require("@decentral.ee/web3-helpers");

describe("SuperTokenFactory Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;

    let superfluid;
    let governance;
    let factory;
    let token1;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 1,
        });

        token1 = await web3tx(TestToken.new, "TestToken.new 1")(
            "Test Token 1",
            "TT1",
            18
        );
        await t.pushEvmSnapshot();

        ({superfluid, governance} = t.contracts);
        factory = await SuperTokenFactory.at(
            await superfluid.getSuperTokenFactory.call()
        );
    });

    after(async () => {
        await t.popEvmSnapshot();
    });

    beforeEach(async function () {
        await t.beforeEachTestCase();
    });

    describe("#1 upgradability", () => {
        it("#1.1 storage layout", async () => {
            const T = artifacts.require("SuperTokenFactoryStorageLayoutTester");
            const tester = await T.new(superfluid.address);
            await tester.validateStorageLayout.call();
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
            await expectRevertedWith(
                factory.updateCode(ZERO_ADDRESS),
                "SuperTokenFactory: only host can update code"
            );
        });

        it("#1.4 only can initialize once", async () => {
            await expectRevertedWith(
                factory.initialize(),
                "Initializable: contract is already initialized"
            );
        });
    });

    describe("#2 createERC20Wrapper", () => {
        context("#2.a Mock factory", () => {
            async function updateSuperTokenFactory() {
                const SuperTokenFactoryMock42 = artifacts.require(
                    "SuperTokenFactoryMock42"
                );
                const helper = await web3tx(
                    SuperTokenFactoryMockHelper.new,
                    "SuperTokenFactoryMockHelper.new"
                )();
                const factory2Logic = await SuperTokenFactoryMock42.new(
                    superfluid.address,
                    helper.address
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
                    upgradability: 0,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                superToken1 = await SuperTokenMock.at(superToken1.address);
                await updateSuperTokenFactory();
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
                await expectRevertedWith(
                    governance.batchUpdateSuperTokenLogic(superfluid.address, [
                        superToken1.address,
                    ]),
                    "UUPSProxiable: not upgradable"
                );
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "0"
                );
            });

            it("#2.a.2 semi upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
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
                    governance.batchUpdateSuperTokenLogic,
                    "governance.batchUpdateSuperTokenLogic"
                )(superfluid.address, [superToken1.address]);
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "42"
                );
            });

            it("#2.a.3 full upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2,
                });
                let proxy = await FullUpgradableSuperTokenProxy.at(
                    superToken1.address
                );
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                superToken1 = await SuperTokenMock.at(superToken1.address);
                await updateSuperTokenFactory();
                assert.equal(
                    (await superToken1.waterMark.call()).toString(),
                    "42"
                );
                await expectRevertedWith(
                    governance.batchUpdateSuperTokenLogic(superfluid.address, [
                        superToken1.address,
                    ]),
                    "UUPSProxiable: not upgradable"
                );
                await expectRevertedWith(
                    proxy.initialize(),
                    "Already initialized"
                );
            });

            it("#2.a.4 Create Custom Token", async () => {
                const CustomSuperTokenMock = artifacts.require(
                    "CustomSuperTokenMock"
                );
                const CustomSuperTokenProxyMock = artifacts.require(
                    "CustomSuperTokenProxyMock"
                );
                const customToken = await CustomSuperTokenMock.at(
                    (
                        await web3tx(
                            CustomSuperTokenProxyMock.new,
                            "CustomSuperTokenProxyMock.new"
                        )(superfluid.address)
                    ).address
                );
                const tx = await web3tx(
                    factory.initializeCustomSuperToken,
                    "initializeCustomSuperToken"
                )(customToken.address);
                await expectEvent(tx.receipt, "CustomSuperTokenCreated", {
                    token: customToken.address,
                });
            });
        });

        context("#2.b Production Factory", () => {
            it("#2.b.1 use production factory to create different super tokens", async () => {
                const helper = await SuperTokenFactoryHelper.new();
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address,
                    helper.address
                );
                await web3tx(
                    governance.updateContracts,
                    "governance.updateContracts"
                )(superfluid.address, ZERO_ADDRESS, [], factory2Logic.address);

                let superToken0 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 0,
                });
                await expectEvent(superToken0.tx.receipt, "SuperTokenCreated", {
                    token: superToken0.address,
                });
                assert.equal(
                    await superToken0.getUnderlyingToken.call(),
                    token1.address
                );

                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                assert.equal(
                    await superToken1.getUnderlyingToken.call(),
                    token1.address
                );

                let superToken2 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2,
                });
                await expectEvent(superToken2.tx.receipt, "SuperTokenCreated", {
                    token: superToken2.address,
                });
                assert.equal(
                    await superToken2.getUnderlyingToken.call(),
                    token1.address
                );
            });
        });

        it("#2.c.1 should fail on ZERO_ADDRESS", async () => {
            await expectRevertedWith(
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
