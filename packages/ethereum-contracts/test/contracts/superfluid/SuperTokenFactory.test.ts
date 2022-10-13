import {artifacts, assert, ethers, expect, web3} from "hardhat";

import {
    SuperfluidMock,
    SuperTokenFactory,
    SuperTokenFactoryMock,
    TestGovernance,
    TestToken,
    TestToken__factory,
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";
import {expectCustomError, expectRevertedWith} from "../../utils/expectRevert";

const {expectEvent} = require("@openzeppelin/test-helpers");

describe("SuperTokenFactory Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    const {ZERO_ADDRESS} = t.constants;

    let superfluid: SuperfluidMock;
    let governance: TestGovernance;
    let factory: SuperTokenFactory;
    let testTokenFactory: TestToken__factory;
    let token1: TestToken;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 1,
        });

        testTokenFactory = await ethers.getContractFactory("TestToken");
        token1 = await testTokenFactory.deploy(
            "Test Token 1",
            "TT1",
            18,
            ethers.utils.parseUnits((1e12).toString())
        );
        await t.pushEvmSnapshot();

        ({superfluid, governance} = t.contracts);
        factory = await ethers.getContractAt(
            "SuperTokenFactory",
            await superfluid.getSuperTokenFactory()
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
            await tester.validateStorageLayout();
        });

        it("#1.2 proxiable info", async () => {
            const proxiable = await ethers.getContractAt(
                "UUPSProxiable",
                factory.address
            );
            assert.equal(
                await proxiable.proxiableUUID(),
                web3.utils.sha3(
                    "org.superfluid-finance.contracts.SuperTokenFactory.implementation"
                )
            );
        });

        it("#1.3 only host can update the code", async () => {
            assert.equal(await factory.getHost(), superfluid.address);
            await expectCustomError(
                factory.updateCode(ZERO_ADDRESS),
                factory,
                "ONLY_HOST",
                t.customErrorCode.SUPER_TOKEN_FACTORY_ONLY_HOST
            );
        });

        it("#1.4 only can initialize once", async () => {
            await expectRevertedWith(
                factory.initialize(),
                "Initializable: contract is already initialized"
            );
        });

        it("#1.5 block initialization of logic contracts", async () => {
            const factoryLogic = await ethers.getContractAt(
                "SuperTokenFactory",
                await factory.getCodeAddress()
            );
            await expectRevertedWith(
                factoryLogic.initialize(),
                "Initializable: contract is already initialized"
            );

            const superTokenLogic = await ethers.getContractAt(
                "SuperTokenMock",
                await factory.getSuperTokenLogic()
            );
            await expectRevertedWith(
                superTokenLogic.initialize(ZERO_ADDRESS, 0, "", ""),
                "Initializable: contract is already initialized"
            );
        });
    });

    describe("#2 createERC20Wrapper", () => {
        context("#2.a Mock factory", () => {
            async function updateSuperTokenFactory() {
                const SuperTokenFactoryMock42 = await ethers.getContractFactory(
                    "SuperTokenFactoryMock42"
                );
                const SuperTokenFactoryMockHelper =
                    await ethers.getContractFactory(
                        "SuperTokenFactoryMockHelper"
                    );
                const helper = await SuperTokenFactoryMockHelper.deploy();
                const factory2Logic = await SuperTokenFactoryMock42.deploy(
                    superfluid.address,
                    helper.address
                );
                await governance.updateContracts(
                    superfluid.address,
                    ZERO_ADDRESS,
                    [],
                    factory2Logic.address
                );
                await superfluid.getSuperTokenFactoryLogic();
            }

            it("#2.a.1 non upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 0,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                superToken1 = await ethers.getContractAt(
                    "SuperTokenMock",
                    superToken1.address
                );
                await updateSuperTokenFactory();
                assert.equal((await superToken1.waterMark()).toString(), "0");
                await expectRevertedWith(
                    governance.batchUpdateSuperTokenLogic(superfluid.address, [
                        superToken1.address,
                    ]),
                    "UUPSProxiable: not upgradable"
                );
                assert.equal((await superToken1.waterMark()).toString(), "0");
            });

            it("#2.a.2 semi upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                superToken1 = await ethers.getContractAt(
                    "SuperTokenMock",
                    superToken1.address
                );
                assert.equal((await superToken1.waterMark()).toString(), "0");
                await updateSuperTokenFactory();
                assert.equal((await superToken1.waterMark()).toString(), "0");
                await governance.batchUpdateSuperTokenLogic(
                    superfluid.address,
                    [superToken1.address]
                );
                assert.equal((await superToken1.waterMark()).toString(), "42");
            });

            it("#2.a.3 full upgradable", async () => {
                let superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2,
                });
                const proxy = await ethers.getContractAt(
                    "FullUpgradableSuperTokenProxy",
                    superToken1.address
                );
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                superToken1 = await ethers.getContractAt(
                    "SuperTokenMock",
                    superToken1.address
                );
                await updateSuperTokenFactory();
                assert.equal((await superToken1.waterMark()).toString(), "42");
                await expectRevertedWith(
                    governance.batchUpdateSuperTokenLogic(superfluid.address, [
                        superToken1.address,
                    ]),
                    "UUPSProxiable: not upgradable"
                );
                await expectCustomError(
                    proxy.initialize(),
                    proxy,
                    "FUSTP_ALREADY_INITIALIZED"
                );
            });

            it("#2.a.4 Create Custom Token", async () => {
                const CustomSuperTokenProxyMockFactory =
                    await ethers.getContractFactory(
                        "CustomSuperTokenProxyMock"
                    );
                const customToken = await ethers.getContractAt(
                    "CustomSuperTokenMock",
                    (
                        await CustomSuperTokenProxyMockFactory.deploy()
                    ).address
                );
                console.log("initializeCustomSuperToken");
                await expect(
                    factory.initializeCustomSuperToken(customToken.address)
                )
                    .to.emit(factory, "CustomSuperTokenCreated")
                    .withArgs(customToken.address);
            });
        });

        context("#2.b Production Factory", () => {
            it("#2.b.1 use production factory to create different super tokens", async () => {
                const helper = await (
                    await ethers.getContractFactory("SuperTokenFactoryHelper")
                ).deploy();
                const factory2Logic = await (
                    await ethers.getContractFactory("SuperTokenFactory")
                ).deploy(superfluid.address, helper.address);
                await governance.updateContracts(
                    superfluid.address,
                    ZERO_ADDRESS,
                    [],
                    factory2Logic.address
                );

                const superToken0 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 0,
                });
                await expectEvent(superToken0.tx.receipt, "SuperTokenCreated", {
                    token: superToken0.address,
                });
                assert.equal(
                    await superToken0.getUnderlyingToken(),
                    token1.address
                );

                const superToken1 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 1,
                });
                await expectEvent(superToken1.tx.receipt, "SuperTokenCreated", {
                    token: superToken1.address,
                });
                assert.equal(
                    await superToken1.getUnderlyingToken(),
                    token1.address
                );

                const superToken2 = await t.sf.createERC20Wrapper(token1, {
                    upgradability: 2,
                });
                await expectEvent(superToken2.tx.receipt, "SuperTokenCreated", {
                    token: superToken2.address,
                });
                assert.equal(
                    await superToken2.getUnderlyingToken(),
                    token1.address
                );
            });
        });

        it("#2.c.1 should fail on ZERO_ADDRESS", async () => {
            await expectCustomError(
                factory[
                    "createERC20Wrapper(address,uint8,uint8,string,string)"
                ](ZERO_ADDRESS, 18, 0, "name", "symbol"),
                factory,
                "ZERO_ADDRESS",
                t.customErrorCode.SUPER_TOKEN_FACTORY_ZERO_ADDRESS
            );
        });

        context("#2.d Canonical Wrapper Super Token Creation", () => {
            context(
                "#2.d.1 Initialize Canonical Wrapper Super Token Function Tests",
                () => {
                    let superTokenFactoryMock: SuperTokenFactoryMock;

                    beforeEach(async () => {
                        const superTokenFactoryMockFactory =
                            await ethers.getContractFactory(
                                "SuperTokenFactoryMock"
                            );
                        const superTokenFactoryMockHelperFactory =
                            await ethers.getContractFactory(
                                "SuperTokenFactoryMockHelper"
                            );
                        const superTokenFactoryMockHelper =
                            await superTokenFactoryMockHelperFactory.deploy();
                        superTokenFactoryMock =
                            await superTokenFactoryMockFactory.deploy(
                                superfluid.address,
                                superTokenFactoryMockHelper.address
                            );
                    });

                    it("#2.d.1a it should throw if not permitted", async () => {
                        const signers = await ethers.getSigners();
                        // only signers[0] is allowed to initialize
                        await expect(
                            superTokenFactoryMock
                                .connect(signers[1])
                                .initializeCanonicalWrapperSuperTokens([])
                        ).to.be.reverted;
                    });

                    it("#2.d.1b it should be able to call initialize canonical wrapper super tokens", async () => {
                        // create deploy test token promises
                        const deployUnderlyingTokenPromises = Array(5)
                            .fill("")
                            .map(async (_x, i) => {
                                return await testTokenFactory.deploy(
                                    `Test Token ${i}`,
                                    `TT${i}`,
                                    18,
                                    ethers.utils.parseUnits((1e12).toString())
                                );
                            });
                        const underlyingTokens = await Promise.all(
                            deployUnderlyingTokenPromises
                        );

                        // create compute wrapper super token address promises
                        const addressPromises = underlyingTokens.map(
                            async (x) => ({
                                underlyingToken: x.address,
                                superToken: (
                                    await factory.computeWrapperSuperTokenAddress(
                                        x.address
                                    )
                                ).superTokenAddress,
                            })
                        );
                        const addresses = await Promise.all(addressPromises);

                        // initialize list of canonical wrappers
                        await superTokenFactoryMock.initializeCanonicalWrapperSuperTokens(
                            addresses
                        );
                    });

                    it("#2.d.1c it should throw if already set", async () => {
                        const computedTokenAddress =
                            await factory.computeWrapperSuperTokenAddress(
                                token1.address
                            );
                        await factory.createCanonicalERC20Wrapper(
                            token1.address
                        );
                        await superTokenFactoryMock.initializeCanonicalWrapperSuperTokens(
                            [
                                {
                                    underlyingToken:
                                        ethers.constants.AddressZero,
                                    superToken:
                                        computedTokenAddress.superTokenAddress,
                                },
                            ]
                        );
                        await expect(
                            superTokenFactoryMock.initializeCanonicalWrapperSuperTokens(
                                []
                            )
                        ).to.be.reverted;
                    });
                }
            );

            it("#2.d.2 it should properly compute address and use create2", async () => {
                const computedTokenAddress =
                    await factory.computeWrapperSuperTokenAddress(
                        token1.address
                    );
                await expect(
                    factory.createCanonicalERC20Wrapper(token1.address)
                )
                    .to.emit(factory, "SuperTokenCreated")
                    .withArgs(computedTokenAddress.superTokenAddress);
                expect(computedTokenAddress.isDeployedAndCanonical).to.equal(
                    false
                );

                const deployedToken =
                    await factory.computeWrapperSuperTokenAddress(
                        token1.address
                    );
                expect(deployedToken.isDeployedAndCanonical).to.equal(true);
            });

            it("#2.d.3 it should not be able to create token twice in a row", async () => {
                const computedTokenAddress =
                    await factory.computeWrapperSuperTokenAddress(
                        token1.address
                    );
                // initial creation should emit event
                await expect(
                    factory.createCanonicalERC20Wrapper(token1.address)
                )
                    .to.emit(factory, "SuperTokenCreated")
                    .withArgs(computedTokenAddress.superTokenAddress);

                // subsequent attempt doesn't throw, but does not create AND does not emit event
                await expect(
                    factory.createCanonicalERC20Wrapper(token1.address)
                ).to.not.emit(factory, "SuperTokenCreated");
            });

            it("#2.d.4 it should be able to create multiple canonical erc20 wrappers", async () => {
                const computedToken1Address =
                    await factory.computeWrapperSuperTokenAddress(
                        token1.address
                    );
                await expect(
                    factory.createCanonicalERC20Wrapper(token1.address)
                )
                    .to.emit(factory, "SuperTokenCreated")
                    .withArgs(computedToken1Address.superTokenAddress);

                const token2 = await testTokenFactory.deploy(
                    "Test Token 2",
                    "TT2",
                    18,
                    ethers.utils.parseUnits((1e12).toString())
                );
                const computedToken2Address =
                    await factory.computeWrapperSuperTokenAddress(
                        token2.address
                    );
                await expect(
                    factory.createCanonicalERC20Wrapper(token2.address)
                )
                    .to.emit(factory, "SuperTokenCreated")
                    .withArgs(computedToken2Address.superTokenAddress);
            });
        });
    });
});
