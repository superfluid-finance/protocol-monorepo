const {expectEvent} = require("@openzeppelin/test-helpers");
const {
    expectRevertedWith,
    expectReverted,
} = require("../../utils/expectRevert");

const SuperfluidMock = artifacts.require("SuperfluidMock");
const AgreementMock = artifacts.require("AgreementMock");
const SuperAppFactoryMock = artifacts.require("SuperAppFactoryMock");
const SuperAppMock = artifacts.require("SuperAppMock");
const SuperAppMockUsingDeprecatedRegisterApp = artifacts.require(
    "SuperAppMockUsingDeprecatedRegisterApp"
);
const SuperAppMockNotSelfRegistering = artifacts.require(
    "SuperAppMockNotSelfRegistering"
);
const TestGovernance = artifacts.require("TestGovernance");
const SuperTokenFactoryHelper = artifacts.require("SuperTokenFactoryHelper");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");

const TestEnvironment = require("../../TestEnvironment");

const {web3tx, toWad, toBN} = require("@decentral.ee/web3-helpers");

describe("Superfluid Host Contract", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let admin, alice, bob;
    const {MAX_UINT256, ZERO_ADDRESS} = t.constants;

    context("Upgradable deployment", () => {
        let governance;
        let superfluid;

        before(async () => {
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 3,
            });

            ({admin, alice, bob} = t.aliases);
            ({superfluid, governance} = t.contracts);
        });

        beforeEach(async function () {
            await t.beforeEachTestCase();
        });

        function createAgreementMock(type, version) {
            return AgreementMock.new(superfluid.address, type, version);
        }

        describe("#1 upgradability", () => {
            it("#1.1 storage layout", async () => {
                const T = artifacts.require("SuperfluidUpgradabilityTester");
                const tester = await T.new();
                await tester.validateStorageLayout.call();
            });

            it("#1.2 proxiable info", async () => {
                assert.equal(
                    await superfluid.proxiableUUID.call(),
                    web3.utils.sha3(
                        "org.superfluid-finance.contracts.Superfluid.implementation"
                    )
                );
            });

            it("#1.3 only governance can update the code", async () => {
                await expectRevertedWith(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#1.4 only can be initialized once", async () => {
                await expectRevertedWith(
                    superfluid.initialize(ZERO_ADDRESS),
                    "Initializable: contract is already initialized"
                );
            });

            it("#1.5 update the code by governance", async () => {
                const mock1 = await SuperfluidMock.new(
                    false /* nonUpgradable */,
                    false /* appWhiteListingEnabled */
                );
                const mock2 = await SuperfluidMock.new(
                    true /* nonUpgradable */,
                    false /* appWhiteListingEnabled */
                );
                await governance.updateContracts(
                    superfluid.address,
                    mock1.address,
                    [],
                    ZERO_ADDRESS
                );

                // don't allow initialization of logic contract
                await expectRevertedWith(
                    mock1.initialize(ZERO_ADDRESS),
                    "Initializable: contract is already initialized"
                );

                assert.equal(await superfluid.getCodeAddress(), mock1.address);
                await expectRevertedWith(
                    governance.updateContracts(
                        superfluid.address,
                        mock2.address,
                        [],
                        ZERO_ADDRESS
                    ),
                    "SF: cannot downgrade to non upgradable"
                );
            });

            it("#1.6 context struct layout", async () => {
                const T = artifacts.require("SuperfluidUpgradabilityTester");
                const tester = await T.new();
                await tester.validateContextStructLayout.call();
            });
        });

        describe("#2 Agreement Whitelisting", async () => {
            it("#2.1 Agreement whitelisting operations", async () => {
                const N_DEFAULT_AGREEMENTS = (
                    await superfluid.mapAgreementClasses.call(MAX_UINT256)
                ).length;
                const typeA = web3.utils.sha3("typeA");
                const typeB = web3.utils.sha3("typeB");
                const mockA = await createAgreementMock(typeA, 1);
                const mockAFake = await createAgreementMock(typeA, 42);
                const mockB = await createAgreementMock(typeB, 1);
                const mockA2 = await createAgreementMock(typeA, 2);

                assert.isFalse(
                    await superfluid.isAgreementTypeListed.call(typeA)
                );
                assert.isFalse(
                    await superfluid.isAgreementTypeListed.call(typeB)
                );
                assert.equal(await mockA.agreementType.call(), typeA);

                // register typeA
                await governance.registerAgreementClass(
                    superfluid.address,
                    mockA.address
                );
                console.debug(
                    "Agreement classes",
                    await superfluid.mapAgreementClasses.call(MAX_UINT256)
                );
                const mockAProxy = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeA)
                );
                assert.equal(await mockAProxy.version.call(), 1);
                assert.equal(
                    await mockA.agreementType.call(),
                    await mockAProxy.agreementType.call()
                );
                assert.isTrue(
                    await superfluid.isAgreementTypeListed.call(typeA)
                );
                assert.isTrue(
                    await superfluid.isAgreementClassListed.call(
                        mockAProxy.address
                    )
                );
                assert.isFalse(
                    await superfluid.isAgreementClassListed.call(mockA.address)
                );
                assert.isFalse(
                    await superfluid.isAgreementClassListed.call(
                        mockAFake.address
                    )
                );
                assert.isFalse(
                    await superfluid.isAgreementTypeListed.call(typeB)
                );
                assert.deepEqual(
                    (
                        await superfluid.mapAgreementClasses.call(MAX_UINT256)
                    ).slice(-1),
                    [mockAProxy.address]
                );

                // register typeB
                await web3tx(
                    governance.registerAgreementClass,
                    "registerAgreementClass typeB"
                )(superfluid.address, mockB.address);
                console.debug(
                    "Agreement classes",
                    await superfluid.mapAgreementClasses.call(MAX_UINT256)
                );
                const mockBProxy = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeB)
                );
                assert.equal(await mockBProxy.version.call(), 1);
                assert.equal(
                    await mockB.agreementType.call(),
                    await mockBProxy.agreementType.call()
                );
                assert.isTrue(
                    await superfluid.isAgreementTypeListed.call(typeA)
                );
                assert.isTrue(
                    await superfluid.isAgreementClassListed.call(
                        mockAProxy.address
                    )
                );
                assert.isTrue(
                    await superfluid.isAgreementTypeListed.call(typeB)
                );
                assert.isTrue(
                    await superfluid.isAgreementClassListed.call(
                        mockBProxy.address
                    )
                );
                assert.deepEqual(
                    (
                        await superfluid.mapAgreementClasses.call(MAX_UINT256)
                    ).slice(-2),
                    [mockAProxy.address, mockBProxy.address]
                );

                // upgrade typeA
                await governance.updateContracts(
                    superfluid.address,
                    ZERO_ADDRESS,
                    [mockA2.address],
                    ZERO_ADDRESS
                );
                console.debug(
                    "Agreement classes",
                    await superfluid.mapAgreementClasses.call(MAX_UINT256)
                );
                const mockAProxy2 = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeA)
                );
                assert.equal(mockAProxy2.address, mockAProxy.address);
                assert.equal(await mockAProxy2.version.call(), 2);

                // bitmap operations
                assert.equal(
                    toBN(MAX_UINT256)
                        .xor(
                            toBN(
                                await superfluid.removeFromAgreementClassesBitmap.call(
                                    MAX_UINT256,
                                    typeA
                                )
                            )
                        )
                        .toString(),
                    toBN(1).shln(N_DEFAULT_AGREEMENTS).toString()
                );
                assert.equal(
                    toBN(MAX_UINT256)
                        .xor(
                            toBN(
                                await superfluid.removeFromAgreementClassesBitmap.call(
                                    MAX_UINT256,
                                    typeB
                                )
                            )
                        )
                        .toString(),
                    toBN(1)
                        .shln(N_DEFAULT_AGREEMENTS + 1)
                        .toString()
                );
                assert.equal(
                    (
                        await superfluid.addToAgreementClassesBitmap(
                            (
                                await superfluid.removeFromAgreementClassesBitmap.call(
                                    MAX_UINT256,
                                    typeA
                                )
                            ).toString(),
                            typeA
                        )
                    ).toString(),
                    MAX_UINT256
                );
            });

            it("#2.2 only governance can update agreement listings", async () => {
                await expectRevertedWith(
                    superfluid.registerAgreementClass(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevertedWith(
                    superfluid.updateAgreementClass(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#2.3 only host can update agreement code", async () => {
                await expectRevertedWith(
                    t.contracts.ida.updateCode(ZERO_ADDRESS),
                    "only host can update code"
                );
            });

            it("#2.4 agreement cannot be registered twice", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await createAgreementMock(typeA, 1);
                const mockA2 = await createAgreementMock(typeA, 2);

                await governance.registerAgreementClass(
                    superfluid.address,
                    mockA.address
                );
                await expectRevertedWith(
                    governance.registerAgreementClass(
                        superfluid.address,
                        mockA2.address
                    ),
                    "SF: agreement class already registered"
                );
            });

            it("#2.5 cannot register more than 256 agreements", async function () {
                const mocks = [];
                mocks.push(t.contracts.cfa.address);
                mocks.push(t.contracts.ida.address);
                for (let i = 0; i < 254; ++i) {
                    process.stdout.write(".");
                    const typeN = web3.utils.sha3("type." + i);
                    const mock = await createAgreementMock(typeN, 1);
                    await governance.registerAgreementClass(
                        superfluid.address,
                        mock.address
                    );
                    mocks.push(await superfluid.getAgreementClass(typeN));
                }
                process.stdout.write("\n");

                const agreements = await superfluid.mapAgreementClasses.call(
                    MAX_UINT256
                );
                for (let i = 0; i < 256; ++i) {
                    assert.equal(
                        agreements[i],
                        mocks[i],
                        `agreement no.${i} mismatch`
                    );
                }

                const badMock = await createAgreementMock(
                    web3.utils.sha3("type.bad"),
                    1
                );
                await expectRevertedWith(
                    governance.registerAgreementClass(
                        superfluid.address,
                        badMock.address
                    ),
                    "SF: support up to 256 agreement classes"
                );
            });

            it("#2.6 agreement must be registered first", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await createAgreementMock(typeA, 1);

                await expectRevertedWith(
                    governance.updateContracts(
                        superfluid.address,
                        ZERO_ADDRESS,
                        [mockA.address],
                        ZERO_ADDRESS
                    ),
                    "SF: agreement class not registered"
                );

                await expectRevertedWith(
                    superfluid.getAgreementClass(typeA),
                    "SF: agreement class not registered"
                );
                await expectRevertedWith(
                    superfluid.addToAgreementClassesBitmap(0, typeA),
                    "SF: agreement class not registered"
                );
                await expectRevertedWith(
                    superfluid.removeFromAgreementClassesBitmap(0, typeA),
                    "SF: agreement class not registered"
                );
            });

            it("#2.7 mapAgreementClasses", async () => {
                const agreements = await superfluid.mapAgreementClasses.call(1);
                assert.equal(agreements.length, 1);
                assert.equal(agreements[0], t.contracts.cfa.address);
            });
        });

        describe("#3 Super Token Factory", () => {
            it("#3.1 only governance can update super token factory", async () => {
                await expectRevertedWith(
                    superfluid.updateSuperTokenFactory(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevertedWith(
                    superfluid.updateSuperTokenLogic(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#3.2 update super token factory", async () => {
                const factory = await superfluid.getSuperTokenFactory();
                const helper = await SuperTokenFactoryHelper.new();
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address,
                    helper.address
                );
                await governance.updateContracts(
                    superfluid.address,
                    ZERO_ADDRESS,
                    [],
                    factory2Logic.address
                );
                assert.equal(
                    await superfluid.getSuperTokenFactory(),
                    factory,
                    "Upgradable factory address does not change"
                );
                assert.equal(
                    await superfluid.getSuperTokenFactoryLogic.call(),
                    factory2Logic.address,
                    "Upgradable factory logic address should change to the new one"
                );
            });
        });

        describe("#4 App Registry", () => {
            let app;

            beforeEach(async () => {
                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );
            });

            it("#4.1 basic app info", async () => {
                assert.isFalse(await superfluid.isApp(governance.address));
                assert.isTrue(await superfluid.isApp(app.address));
                assert.isFalse(await superfluid.isAppJailed(app.address));
                assert.equal(await superfluid.getAppLevel(app.address), 1);
                const manifest = await superfluid.getAppManifest(app.address);
                assert.equal(manifest.isSuperApp, true);
                assert.equal(manifest.isJailed, false);
                assert.equal(manifest.noopMask, 0);
                const nomanifest = await superfluid.getAppManifest(admin);
                assert.equal(nomanifest.isSuperApp, false);
            });

            it("#4.2 app registration rules", async () => {
                await expectRevertedWith(
                    superfluid.registerApp(1, {from: admin}),
                    "SF: APP_RULE_NO_REGISTRATION_FOR_EOA"
                );
                await expectRevertedWith(
                    app.tryRegisterApp(0),
                    "SF: APP_RULE_REGISTRATION_ONLY_IN_CONSTRUCTOR"
                );
            });

            it("#4.3 app registration with bad config", async () => {
                const reason = "SF: invalid config word";
                await expectRevertedWith(
                    SuperAppMock.new(superfluid.address, 0, false),
                    reason
                );
                await expectRevertedWith(
                    SuperAppMock.new(
                        superfluid.address,
                        1 | (1 << 15) /* jail bit */,
                        false
                    ),
                    reason
                );
                await expectRevertedWith(
                    SuperAppMock.new(
                        superfluid.address,
                        1 | (1 << 16) /* garbage bit */,
                        false
                    ),
                    reason
                );
            });

            it("#4.4 app double registration should fail", async () => {
                await expectRevertedWith(
                    SuperAppMock.new(superfluid.address, 1, true),
                    "SF: app already registered"
                );
            });

            it("#4.5 allowCompositeApp", async () => {
                const app2 = await SuperAppMock.new(
                    superfluid.address,
                    2 /* APP_LEVEL_SECOND */,
                    false
                );
                await expectRevertedWith(
                    superfluid.allowCompositeApp(app.address),
                    "SF: sender is not an app"
                );
                await expectRevertedWith(
                    app.allowCompositeApp(alice),
                    "SF: target is not an app"
                );
                await expectRevertedWith(
                    app.allowCompositeApp(app2.address),
                    "SF: source app should have higher app level"
                );
                assert.isFalse(
                    await superfluid.isCompositeAppAllowed.call(
                        app.address,
                        app2.address
                    )
                );
                await app2.allowCompositeApp(app.address);
                assert.isTrue(
                    await superfluid.isCompositeAppAllowed.call(
                        app2.address,
                        app.address
                    )
                );
            });

            it("#4.6 deprecated register app should continues to work", async () => {
                const app2 = await SuperAppMockUsingDeprecatedRegisterApp.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */
                );
                assert.isTrue(await superfluid.isApp(app2.address));
            });

            it("#4.7 app registration as factory by EOA should fail", async () => {
                await expectRevertedWith(
                    superfluid.registerAppByFactory(ZERO_ADDRESS, 1),
                    "SF: factory must be a contract"
                );
            });

            it("#4.8 register app by factory", async () => {
                const appFactory = await SuperAppFactoryMock.new();
                // since whitelisting is disabled, so governance ain't required
                const app = await SuperAppMockNotSelfRegistering.new();
                assert.isFalse(await superfluid.isApp(app.address));
                await appFactory.registerAppWithHost(
                    superfluid.address,
                    app.address,
                    1 /* APP_TYPE_FINAL_LEVEL */
                );
                assert.isTrue(await superfluid.isApp(app.address));
            });
        });

        describe("#5 Context Utilities", () => {
            it("#5.1 test replacePlaceholderCtx with testCtxFuncX", async () => {
                const testCtxFunc = async (ctxFuncX, args, ctx) => {
                    const result = (
                        await superfluid.testCtxFuncX(
                            superfluid.contract.methods[ctxFuncX](
                                ...args,
                                "0x"
                            ).encodeABI(),
                            ctx
                        )
                    ).slice(2);
                    const expectedResult = superfluid.contract.methods[
                        ctxFuncX
                    ](...args, ctx)
                        .encodeABI()
                        .slice(10);
                    assert.equal(result, expectedResult);
                };

                // test the boundary conditions
                for (let i = 0; i < 33; ++i) {
                    // with 32 bytes boundary padding
                    await testCtxFunc("ctxFunc1", [42], "0x" + "d".repeat(i));
                }

                // more complicated ABI
                await testCtxFunc(
                    "ctxFunc2",
                    [
                        governance.address,
                        t.contracts.ida.address,
                        "0x2020",
                        "0x" /* agreementData */,
                        "0x" /* cbdata */,
                    ],
                    "0x" + "dead".repeat(20)
                );
                await testCtxFunc(
                    "ctxFunc2",
                    [
                        governance.address,
                        t.contracts.ida.address,
                        "0x2020",
                        "0xdead" /* agreementData */,
                        "0xbeef" /* cbdata */,
                    ],
                    "0x" + "faec".repeat(20)
                );

                // error case
                await expectRevertedWith(
                    superfluid.testCtxFuncX(
                        superfluid.contract.methods
                            .ctxFunc1(42, "0xbad")
                            .encodeABI(),
                        "0xbeef"
                    ),
                    "SF: placeholder ctx should have zero length"
                );
            });
        });

        describe("#6 Agreement Framework", () => {
            let agreement;
            let app;
            let gasLimit;

            before(async () => {
                await t.useLastEvmSnapshot();

                gasLimit = (
                    await superfluid.CALLBACK_GAS_LIMIT.call()
                ).toString();
                agreement = await createAgreementMock(
                    web3.utils.sha3("MockAgreement"),
                    0
                );
                await governance.registerAgreementClass(
                    superfluid.address,
                    agreement.address
                );
                agreement = await AgreementMock.at(
                    await superfluid.getAgreementClass(
                        web3.utils.sha3("MockAgreement")
                    )
                );

                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                await t.pushEvmSnapshot();
            });

            after(async () => {
                await t.popEvmSnapshot();
            });

            context("#6.x agreement framework access control", () => {
                it("#6.1 use agreement framework as an EOA", async () => {
                    await expectReverted(
                        superfluid.callAppBeforeCallback(
                            ZERO_ADDRESS,
                            "0x",
                            false,
                            "0x"
                        )
                    );
                    await expectReverted(
                        superfluid.callAppAfterCallback(
                            ZERO_ADDRESS,
                            "0x",
                            false,
                            "0x"
                        )
                    );
                    await expectReverted(
                        superfluid.appCallbackPush(
                            "0x",
                            ZERO_ADDRESS,
                            0,
                            0,
                            ZERO_ADDRESS
                        )
                    );
                    await expectReverted(superfluid.appCallbackPop("0x", 0));
                    await expectReverted(superfluid.ctxUseCredit("0x", 0));
                });

                it("#6.2 use agreement framework as an unregistered agreement", async () => {
                    const reason = "SF: sender is not listed agreement";

                    // call from an unregistered mock agreement
                    const mock = await createAgreementMock(
                        web3.utils.sha3("typeA"),
                        0
                    );
                    await expectRevertedWith(
                        mock.tryCallAppBeforeCallback(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryCallAppAfterCallback(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryAppCallbackPush(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryAppCallbackPop(superfluid.address, "0x"),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryCtxUseCredit(
                            superfluid.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryJailApp(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                });

                it("#6.3 use agreement framework as an impersonating agreement", async () => {
                    const reason = "SF: sender is not listed agreement";

                    const mock = await createAgreementMock(
                        await t.contracts.cfa.agreementType.call(),
                        0
                    );
                    await expectRevertedWith(
                        mock.tryCallAppBeforeCallback(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryCallAppAfterCallback(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryAppCallbackPush(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryAppCallbackPop(superfluid.address, "0x"),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryCtxUseCredit(
                            superfluid.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                    await expectRevertedWith(
                        mock.tryJailApp(
                            superfluid.address,
                            app.address,
                            false /* do not hack Ctx */,
                            "0x"
                        ),
                        reason
                    );
                });

                it("#6.4 callback will not be called for jailed apps", async () => {
                    await superfluid.jailApp(app.address);
                    await app.setNextCallbackAction(1 /* assert */, "0x");
                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementCreatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                });

                it("#6.5 bad agreement implementations", async () => {
                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryCallAppBeforeCallback(
                                superfluid.address,
                                app.address,
                                false /* do not hack Ctx */,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectReverted(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .tryCallAppBeforeCallback(
                                    superfluid.address,
                                    app.address,
                                    true /* hack the Ctx */,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        )
                    );

                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryCallAppAfterCallback(
                                superfluid.address,
                                app.address,
                                false /* do not hack Ctx */,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectReverted(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .tryCallAppAfterCallback(
                                    superfluid.address,
                                    app.address,
                                    true /* hack the Ctx */,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        )
                    );

                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryAppCallbackPush(
                                superfluid.address,
                                app.address,
                                false /* hack the Ctx */,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectReverted(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .tryAppCallbackPush(
                                    superfluid.address,
                                    app.address,
                                    true /* hack the Ctx */,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        )
                    );

                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryAppCallbackPop(superfluid.address, "0x")
                            .encodeABI(),
                        "0x"
                    );
                    // NOTE that tryAppCallbackPop cannot be protected YET

                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryCtxUseCredit(
                                superfluid.address,
                                false /* hack the Ctx */,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectReverted(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .tryCtxUseCredit(
                                    superfluid.address,
                                    true /* hack the Ctx */,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        )
                    );

                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .tryJailApp(
                                superfluid.address,
                                app.address,
                                false /* hack the Ctx */,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectReverted(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .tryJailApp(
                                    superfluid.address,
                                    app.address,
                                    true /* hack the Ctx */,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        )
                    );
                });
            });

            context("#6.1x agreement callback rules", async () => {
                it("#6.10 beforeAgreementCreated callback noop should work", async () => {
                    await app.setNextCallbackAction(0 /* noop */, "0x");
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppBeforeAgreementCreatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    await expectEvent.inTransaction(
                        tx.tx,
                        agreement.contract,
                        "AppBeforeCallbackResult",
                        {
                            appLevel: "0",
                            callType: "1" /* CALL_INFO_CALL_TYPE_AGREEMENT */,
                            agreementSelector: agreement.abi.filter(
                                (i) =>
                                    i.name ===
                                    "callAppBeforeAgreementCreatedCallback"
                            )[0].signature,
                            cbdata: "0x" + Buffer.from("Noop").toString("hex"),
                        }
                    );
                });

                it("#6.11 beforeAgreementCreated callback assert or revert", async () => {
                    await app.setNextCallbackAction(1 /* assert */, "0x");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "CallUtils: target panicked: 0x01"
                    );

                    await app.setNextCallbackAction(2 /* revert */, "0x");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "CallUtils: target revert()"
                    );

                    await app.setNextCallbackAction(
                        3 /* revert with reason */,
                        web3.eth.abi.encodeParameter("string", "error 42")
                    );
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "error 42"
                    );
                });

                it("#6.12 afterAgreementCreated callback noop should work", async () => {
                    await app.setNextCallbackAction(0 /* noop */, "0x");
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementCreatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    const agreementSelector = agreement.abi.filter(
                        (i) => i.name === "callAppAfterAgreementCreatedCallback"
                    )[0].signature;
                    await expectEvent.inTransaction(
                        tx.tx,
                        app.contract,
                        "NoopEvent",
                        {
                            appLevel: "1",
                            callType:
                                "3" /* CALL_INFO_CALL_TYPE_APP_CALLBACK */,
                            agreementSelector,
                        }
                    );
                    await expectEvent.inTransaction(
                        tx.tx,
                        agreement.contract,
                        "AppAfterCallbackResult",
                        {
                            appLevel: "0",
                            callType: "1" /* CALL_INFO_CALL_TYPE_AGREEMENT */,
                            agreementSelector,
                        }
                    );
                });

                it("#6.13 afterAgreementCreated callback assert or revert", async () => {
                    await app.setNextCallbackAction(1 /* assert */, "0x");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "CallUtils: target panicked: 0x01"
                    );

                    await app.setNextCallbackAction(2 /* revert */, "0x");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "CallUtils: target revert()"
                    );

                    await app.setNextCallbackAction(
                        3 /* revert with reason */,
                        web3.eth.abi.encodeParameter("string", "error 42")
                    );
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "error 42"
                    );
                });

                it("#6.14 afterAgreementCreated callback altering ctx", async () => {
                    await app.setNextCallbackAction(4 /* AlteringCtx */, "0x");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_CTX_IS_READONLY"
                    );
                });

                it("#6.15 beforeAgreementTerminated callback revert jail rule", async () => {
                    await app.setNextCallbackAction(1 /* assert */, "0x");
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppBeforeAgreementTerminatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "10", // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                        }
                    );
                });

                it("#6.16 afterAgreementTerminated callback revert jail rule", async () => {
                    await app.setNextCallbackAction(1 /* assert */, "0x");
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementTerminatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "10", // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                        }
                    );
                });

                it("#6.17 afterAgreementTerminated callback readonly ctx jail rule", async () => {
                    await app.setNextCallbackAction(4 /* AlteringCtx */, "0x");
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementTerminatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "20", // APP_RULE_CTX_IS_READONLY
                        }
                    );
                });

                it("#6.18 agreement callback noops masks", async () => {
                    const tests = [
                        [0, "callAppBeforeAgreementCreatedCallback"],
                        [1, "callAppAfterAgreementCreatedCallback"],
                        [2, "callAppBeforeAgreementUpdatedCallback"],
                        [3, "callAppAfterAgreementUpdatedCallback"],
                        [4, "callAppBeforeAgreementTerminatedCallback"],
                        [5, "callAppAfterAgreementTerminatedCallback"],
                    ];
                    for (let i = 0; i < tests.length; ++i) {
                        console.debug("testing noop mask for", tests[i][1]);
                        let app2 = await SuperAppMock.new(
                            superfluid.address,
                            /* APP_TYPE_FINAL_LEVEL */
                            toBN(1).add(
                                // *_NOOP:  1 << (32 + n)
                                toBN(1).shln(32 + tests[i][0])
                            ),
                            false
                        );
                        await app2.setNextCallbackAction(1 /* assert */, "0x");
                        await superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods[tests[i][1]](
                                app2.address,
                                "0x"
                            ).encodeABI(),
                            "0x"
                        );
                    }
                });

                it("#6.19 Jail event should not be emitted twice", async () => {
                    let tx = await superfluid.jailApp(app.address);
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "6942", // it is scientific, check the code
                        }
                    );
                    tx = await superfluid.jailApp(app.address);
                    await expectEvent.notEmitted.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail"
                    );
                });
            });

            context("#6.2x callback gas limit", () => {
                it("#6.20 beforeCreated callback burn all gas", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // burn all the gas
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "CallUtils: target revert()"
                    );
                });

                it("#6.21 beforeCreated callback try to burn all gas but less gas provided", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // provide less gas
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x",
                            {
                                gas: Math.ceil(gasLimit / 2),
                            }
                        ),
                        "SF: need more gas"
                    );
                });

                it("#6.22 afterTerminated burn all gas", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // provide less gas
                    const tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementTerminatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x",
                        {
                            // give enough gas to trigger the error
                            gas: gasLimit * 2,
                        }
                    );
                    assert.isTrue(await superfluid.isAppJailed(app.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "10", // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                        }
                    );
                });

                it("#6.23 afterTerminated try to burn all gas but with less gas provided", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // provide less gas
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementTerminatedCallback(
                                    app.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x",
                            {
                                gas: Math.ceil(gasLimit / 2),
                            }
                        ),
                        "SF: need more gas"
                    );
                });

                it("#6.24 beforeCreated try to burn just enough gas [ @skip-on-coverage ]", async function () {
                    const actionOverhead = 20000; /* some action overhead */
                    const setNextAction = async () => {
                        await app.setNextCallbackAction(
                            5 /* BurnGas */,
                            web3.eth.abi.encodeParameter(
                                "uint256",
                                gasLimit - actionOverhead
                            )
                        );
                    };
                    await setNextAction();
                    let tx = await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppBeforeAgreementCreatedCallback(
                                app.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    console.debug("Gas used", tx.receipt.gasUsed);
                    let gasLowerBound = tx.receipt.gasUsed;
                    let gasUpperBound = gasLowerBound + 300000;
                    console.debug(
                        "Current bound",
                        gasLowerBound,
                        gasUpperBound
                    );

                    // binary search proof if there is a price can trigger unexpected revert
                    let gas;
                    let errorCount = 0;
                    let successCount = 0;
                    while (gasLowerBound <= gasUpperBound) {
                        const gap = Math.floor(
                            (gasUpperBound - gasLowerBound) / 2
                        );
                        gas = gasLowerBound + gap;
                        console.debug("Trying with new gas limit", gas);
                        try {
                            await setNextAction();
                            tx = await superfluid.callAgreement(
                                agreement.address,
                                agreement.contract.methods
                                    .callAppBeforeAgreementCreatedCallback(
                                        app.address,
                                        "0x"
                                    )
                                    .encodeABI(),
                                "0x",
                                {
                                    gas,
                                }
                            );
                            console.debug("Gas used", tx.receipt.gasUsed);
                            console.debug(
                                "No error, decreasing gas with gap",
                                gasLowerBound,
                                gas,
                                gasUpperBound
                            );
                            gasUpperBound = gas;
                            ++errorCount;
                        } catch (error) {
                            // with error, check error and increase gas
                            assert.isNotNull(
                                error.message.match("SF: need more gas")
                            );
                            console.debug(
                                "Caught error, increasing gas with gap",
                                gasLowerBound,
                                gas,
                                gasUpperBound
                            );
                            gasLowerBound = gas;
                            ++successCount;
                        }
                        if (gap === 0) break;
                    }
                    assert.isTrue(errorCount > 0, "expect some errors");
                    assert.isTrue(successCount > 0, "expect some success");
                });
            });

            context("#6.3x composite app rules", () => {
                const SuperAppMock2ndLevel = artifacts.require(
                    "SuperAppMock2ndLevel"
                );

                it("#6.30 composite app must be whitelisted", async () => {
                    // assuming MAX_APP_LEVEL = 1
                    // -> mockAgreement.callAppAfterAgreementCreatedCallback(app3)
                    // -> app3.afterAgreementCreated
                    // -> mockAgreement.callAppAfterAgreementCreatedCallback(app)
                    const app3 = await SuperAppMock2ndLevel.new(
                        superfluid.address,
                        app.address,
                        agreement.address
                    );
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app3.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED"
                    );
                    await app3.allowCompositeApp();
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app3.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_MAX_APP_LEVEL_REACHED"
                    );
                });

                it("#6.31 composite app cannot be jailed", async () => {
                    // assuming MAX_APP_LEVEL = 1
                    // -> mockAgreement.callAppAfterAgreementCreatedCallback(app3)
                    // -> app3.afterAgreementCreated
                    // -> mockAgreement.callAppAfterAgreementCreatedCallback(app) // jailed
                    const app3 = await SuperAppMock2ndLevel.new(
                        superfluid.address,
                        app.address,
                        agreement.address
                    );
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app3.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_COMPOSITE_APP_IS_NOT_WHITELISTED"
                    );
                    await superfluid.jailApp(app.address);
                    await superfluid.callAgreement(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementCreatedCallback(
                                app3.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                });
            });

            context("#6.4x invalid ctx returned by the callback", () => {
                const SuperAppMock2 = artifacts.require(
                    "SuperAppMockReturningEmptyCtx"
                );
                const SuperAppMock2ndLevel = artifacts.require(
                    "SuperAppMockReturningInvalidCtx"
                );

                it("#6.40 should give explicit error message in non-termination callbacks", async () => {
                    const app2 = await SuperAppMock2.new(superfluid.address);

                    console.debug("callAppBeforeAgreementCreatedCallback");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppBeforeAgreementCreatedCallback(
                                    app2.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_CTX_IS_MALFORMATED"
                    );

                    console.debug("callAppAfterAgreementCreatedCallback");
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app2.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_CTX_IS_MALFORMATED"
                    );

                    console.debug("callAppAfterAgreementCreatedCallback");
                    const app3 = await SuperAppMock2ndLevel.new(
                        superfluid.address
                    );
                    await expectRevertedWith(
                        superfluid.callAgreement(
                            agreement.address,
                            agreement.contract.methods
                                .callAppAfterAgreementCreatedCallback(
                                    app3.address,
                                    "0x"
                                )
                                .encodeABI(),
                            "0x"
                        ),
                        "SF: APP_RULE_CTX_IS_MALFORMATED"
                    );
                });

                it("#6.41 should jail the app in termination callbacks", async () => {
                    let app2;
                    let tx;

                    app2 = await SuperAppMock2.new(superfluid.address);
                    tx = await web3tx(
                        superfluid.callAgreement,
                        "callAppBeforeAgreementTerminatedCallback"
                    )(
                        agreement.address,
                        agreement.contract.methods
                            .callAppBeforeAgreementTerminatedCallback(
                                app2.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app2.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app2.address,
                            reason: "22", // APP_RULE_CTX_IS_MALFORMATED
                        }
                    );

                    app2 = await SuperAppMock2.new(superfluid.address);
                    tx = await web3tx(
                        superfluid.callAgreement,
                        "callAppAfterAgreementTerminatedCallback"
                    )(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementTerminatedCallback(
                                app2.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app2.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app2.address,
                            reason: "22", // APP_RULE_CTX_IS_MALFORMATED
                        }
                    );

                    const app3 = await SuperAppMock2ndLevel.new(
                        superfluid.address
                    );
                    tx = await web3tx(
                        superfluid.callAgreement,
                        "callAppAfterAgreementTerminatedCallback"
                    )(
                        agreement.address,
                        agreement.contract.methods
                            .callAppAfterAgreementTerminatedCallback(
                                app3.address,
                                "0x"
                            )
                            .encodeABI(),
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app2.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app3.address,
                            reason: "22", // APP_RULE_CTX_IS_MALFORMATED
                        }
                    );
                });
            });
        });

        describe("#7 callAgreement", () => {
            it("#7.1 only listed agreement allowed", async () => {
                const reason = "SF: only listed agreement allowed";
                // call to an non agreement
                await expectRevertedWith(
                    superfluid.callAgreement(alice, "0x", "0x"),
                    "revert"
                );
                // call to an unregistered mock agreement
                let mock = await createAgreementMock(
                    web3.utils.sha3("typeA"),
                    0
                );
                await expectRevertedWith(
                    superfluid.callAgreement(mock.address, "0x", "0x"),
                    reason
                );
                // call to an in personating mock agreement
                mock = await createAgreementMock(
                    await t.contracts.cfa.agreementType.call(),
                    0
                );
                await expectRevertedWith(
                    superfluid.callAgreement(mock.address, "0x", "0x"),
                    reason
                );
            });

            it("#7.2 callData without correct selector", async () => {
                await expectRevertedWith(
                    superfluid.callAgreement(
                        t.contracts.cfa.address,
                        "0x",
                        "0x"
                    ),
                    "CallUtils: invalid callData"
                );
            });
        });

        describe("#8 callAppAction", () => {
            let agreement;
            let app;

            before(async () => {
                await t.useLastEvmSnapshot();

                agreement = await createAgreementMock(
                    web3.utils.sha3("MockAgreement"),
                    0
                );
                await governance.registerAgreementClass(
                    superfluid.address,
                    agreement.address
                );
                agreement = await AgreementMock.at(
                    await superfluid.getAgreementClass(
                        web3.utils.sha3("MockAgreement")
                    )
                );

                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                await t.pushEvmSnapshot();
            });

            after(async () => {
                await t.popEvmSnapshot();
            });

            it("#8.1 only super app can be called", async () => {
                const reason = "SF: not a super app";
                // call to an non agreement
                await expectRevertedWith(
                    superfluid.callAppAction(alice, "0x"),
                    "revert"
                );
                // call to an unregistered mock agreement
                await expectRevertedWith(
                    superfluid.callAppAction(governance.address, "0x"),
                    reason
                );
            });

            it("#8.2 actionNoop", async () => {
                const tx = await superfluid.callAppAction(
                    app.address,
                    app.contract.methods.actionNoop("0x").encodeABI()
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent",
                    {
                        appLevel: "0",
                        callType: "2" /* CALL_INFO_CALL_TYPE_APP_ACTION */,
                        agreementSelector: "0x00000000",
                    }
                );
            });

            it("#8.3 callAppAction assert or revert", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionAssert("0x").encodeABI()
                    ),
                    "CallUtils: target panicked: 0x01"
                );
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionRevert("0x").encodeABI()
                    ),
                    "CallUtils: target revert()"
                );
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionRevertWithReason("error 42", "0x")
                            .encodeABI()
                    ),
                    "error 42"
                );
            });

            it("#8.4 app action should not callAgreement or callAppAction without ctx", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallAgreementWithoutCtx("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_NOT_CLEAN"
                );
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallAppActionWithoutCtx("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_NOT_CLEAN"
                );
            });

            it("#8.5 app callAgreementWithContext which doPing", async () => {
                const tx = await superfluid.callAppAction(
                    app.address,
                    app.contract.methods
                        .actionPingAgreement(agreement.address, 42, "0x")
                        .encodeABI()
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "42",
                    }
                );
            });

            it("#8.6 app callAgreementWithContext which reverts", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionAgreementRevert(
                                agreement.address,
                                "error 42",
                                "0x"
                            )
                            .encodeABI()
                    ),
                    "error 42"
                );
            });

            it("#8.7 app callAppActionWithContext which noop", async () => {
                const tx = await superfluid.callAppAction(
                    app.address,
                    app.contract.methods.actionCallActionNoop("0x").encodeABI()
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent"
                );
            });

            it("#8.8 app callAppActionWithContext which reverts", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallActionRevert("error 42", "0x")
                            .encodeABI()
                    ),
                    "error 42"
                );
            });

            it("#8.9 app action should not alter ctx", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionAlteringCtx("0x").encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_READONLY"
                );
            });

            it("#8.10 should not be able call jailed app", async () => {
                await superfluid.jailApp(app.address);
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallActionNoop("0x")
                            .encodeABI()
                    ),
                    "SF: app is jailed"
                );
            });

            it("#8.11 should give explicit error message when empty ctx returned by the action", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionReturnEmptyCtx("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_EMPTY"
                );
            });

            it("#8.12 should not be able to call callbacks", async () => {
                await Promise.all(
                    [
                        "beforeAgreementCreated",
                        "afterAgreementCreated",
                        "beforeAgreementUpdated",
                        "afterAgreementUpdated",
                        "beforeAgreementTerminated",
                        "afterAgreementTerminated",
                    ].map(async (cbname) => {
                        const sel = app.abi.filter((i) => i.name == cbname)[0]
                            .signature;
                        await expectRevertedWith(
                            superfluid.callAppAction(app.address, sel),
                            "SF: agreement callback is not action"
                        );
                    })
                );
            });
        });

        describe("#9 Contextual Call Proxies", () => {
            let agreement;
            let app;

            before(async () => {
                await t.useLastEvmSnapshot();

                agreement = await createAgreementMock(
                    web3.utils.sha3("MockAgreement"),
                    0
                );
                await governance.registerAgreementClass(
                    superfluid.address,
                    agreement.address
                );
                agreement = await AgreementMock.at(
                    await superfluid.getAgreementClass(
                        web3.utils.sha3("MockAgreement")
                    )
                );

                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                await t.pushEvmSnapshot();
            });

            after(async () => {
                await t.popEvmSnapshot();
            });

            it("#9.1 must call with valid ctx", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallAgreementWithInvalidCtx(
                                agreement.address,
                                "0x"
                            )
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_NOT_VALID"
                );
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallActionWithInvalidCtx(
                                agreement.address,
                                "0x"
                            )
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_NOT_VALID"
                );
            });

            it("#9.2 app action should not alter ctx", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallBadAction("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_READONLY"
                );
            });

            it("#9.3 callAgreementWithContext should from the same app", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionPingAgreementThroughAux(
                                agreement.address,
                                42,
                                "0x"
                            )
                            .encodeABI()
                    ),
                    "SF: callAgreementWithContext from wrong address"
                );
            });

            it("#9.4 callAppActionWithContext should from the same app", async () => {
                await expectRevertedWith(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallActionNoopThroughAux("0x")
                            .encodeABI()
                    ),
                    "SF: callAppActionWithContext from wrong address"
                );
            });
        });

        describe("#10 batchCall", () => {
            it("#10.1 batchCall upgrade/approve/transfer/downgrade in one", async () => {
                const superToken = t.sf.tokens.TESTx;

                await web3tx(superToken.upgrade, "Alice upgrades 10 tokens")(
                    toWad("10"),
                    {
                        from: alice,
                    }
                );

                await web3tx(
                    superToken.approve,
                    "SuperToken.approve - from alice to admin"
                )(admin, toWad("3"), {
                    from: alice,
                });
                assert.equal(
                    (await superToken.allowance.call(alice, admin)).toString(),
                    toWad("3").toString()
                );

                await web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                    [
                        [
                            101, // upgrade
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["uint256"],
                                [toWad("10").toString()]
                            ),
                        ],
                        [
                            1, // approve
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "uint256"],
                                [bob, toWad("1").toString()]
                            ),
                        ],
                        [
                            2, // transferFrom own funds
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "address", "uint256"],
                                [admin, bob, toWad("2").toString()]
                            ),
                        ],
                        [
                            2, // transferFrom other's funds
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "address", "uint256"],
                                [alice, bob, toWad("3").toString()]
                            ),
                        ],
                        [
                            102, // downgrade
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["uint256"],
                                [toWad("5").toString()]
                            ),
                        ],
                    ],
                    {
                        from: admin,
                    }
                );
                assert.equal(
                    (await superToken.balanceOf.call(admin)).toString(),
                    toWad("3").toString()
                );
                assert.equal(
                    (await superToken.allowance.call(alice, admin)).toString(),
                    toWad("0").toString()
                );
                assert.equal(
                    (await superToken.balanceOf.call(alice)).toString(),
                    toWad("7").toString()
                );
                assert.equal(
                    (await superToken.allowance.call(admin, bob)).toString(),
                    toWad("1").toString()
                );
                assert.equal(
                    (await superToken.balanceOf.call(bob)).toString(),
                    toWad("5").toString()
                );

                await t.validateSystemInvariance();
            });

            it("#10.2 batchCall call agreement", async () => {
                let agreement = await createAgreementMock(
                    web3.utils.sha3("MockAgreement"),
                    0
                );
                await web3tx(
                    governance.registerAgreementClass,
                    "Registering mock agreement"
                )(superfluid.address, agreement.address);
                agreement = await AgreementMock.at(
                    await superfluid.getAgreementClass(
                        web3.utils.sha3("MockAgreement")
                    )
                );

                const tx = await web3tx(
                    superfluid.batchCall,
                    "Superfluid.batchCall"
                )(
                    [
                        [
                            201, // call agreement
                            agreement.address,
                            web3.eth.abi.encodeParameters(
                                ["bytes", "bytes"],
                                [
                                    agreement.contract.methods
                                        .pingMe(admin, 42, "0x")
                                        .encodeABI(),
                                    "0x", // user data
                                ]
                            ),
                        ],
                        [
                            201, // call agreement
                            agreement.address,
                            web3.eth.abi.encodeParameters(
                                ["bytes", "bytes"],
                                [
                                    agreement.contract.methods
                                        .pingMe(admin, 43, "0x")
                                        .encodeABI(),
                                    "0x", // user data
                                ]
                            ),
                        ],
                    ],
                    {
                        from: admin,
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "42",
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "43",
                    }
                );
            });

            it("#10.3 batchCall call app action", async () => {
                let agreement = await createAgreementMock(
                    web3.utils.sha3("MockAgreement"),
                    0
                );
                await web3tx(
                    governance.registerAgreementClass,
                    "Registering mock agreement"
                )(superfluid.address, agreement.address);
                agreement = await AgreementMock.at(
                    await superfluid.getAgreementClass(
                        web3.utils.sha3("MockAgreement")
                    )
                );
                const app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                const tx = await web3tx(
                    superfluid.batchCall,
                    "Superfluid.batchCall"
                )(
                    [
                        [
                            201, // call agreement
                            agreement.address,
                            web3.eth.abi.encodeParameters(
                                ["bytes", "bytes"],
                                [
                                    agreement.contract.methods
                                        .pingMe(admin, 42, "0x")
                                        .encodeABI(),
                                    "0x", // user data
                                ]
                            ),
                        ],
                        [
                            202, // call app action
                            app.address,
                            app.contract.methods.actionNoop("0x").encodeABI(),
                        ],
                    ],
                    {
                        from: admin,
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "42",
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent"
                );
            });

            it("#10.4 batchCall one fail revert all", async () => {
                const app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                await expectRevertedWith(
                    web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                        [
                            [
                                202, // call app action
                                app.address,
                                app.contract.methods
                                    .actionCallActionNoop("0x")
                                    .encodeABI(),
                            ],
                            [
                                202, // call app action
                                app.address,
                                app.contract.methods
                                    .actionCallActionRevert("error 42", "0x")
                                    .encodeABI(),
                            ],
                        ],
                        {
                            from: admin,
                        }
                    ),
                    "error 42"
                );
            });

            it("#10.5 batchCall invalid operation type", async () => {
                await expectRevertedWith(
                    web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                        [[8888, ZERO_ADDRESS, "0x"]],
                        {
                            from: admin,
                        }
                    ),
                    "SF: unknown batch call operation type"
                );
            });
        });

        describe("#11 forwardBatchCall", () => {
            const ForwarderMock = artifacts.require("ForwarderMock");

            let forwarder;

            beforeEach(async () => {
                forwarder = await ForwarderMock.new();
            });

            it("#11.1 forwardBatchCall with mocked transaction signer", async () => {
                await governance.enableTrustedForwarder(
                    superfluid.address,
                    ZERO_ADDRESS,
                    forwarder.address,
                    {
                        from: admin,
                    }
                );
                const superToken = t.sf.tokens.TESTx;
                await t.upgradeBalance("alice", toWad(1));
                await web3tx(forwarder.execute, "forwarder.execute")(
                    {
                        from: alice, // mocked transaction signer
                        to: superfluid.address,
                        value: "0",
                        gas: "5000000",
                        data: superfluid.contract.methods
                            .forwardBatchCall([
                                [
                                    2, // OPERATION_TYPE_ERC20_TRANSFER_FROM
                                    superToken.address,
                                    web3.eth.abi.encodeParameters(
                                        ["address", "address", "uint256"],
                                        [alice, bob, toWad(1).toString()]
                                    ),
                                ],
                            ])
                            .encodeABI(),
                    },
                    {from: admin}
                );
                assert.equal(
                    (await superToken.balanceOf(bob)).toString(),
                    toWad(1).toString()
                );
            });

            it("#11.2 untrusted forwarder", async () => {
                await governance.disableTrustedForwarder(
                    superfluid.address,
                    ZERO_ADDRESS,
                    forwarder.address,
                    {
                        from: admin,
                    }
                );
                await expectRevertedWith(
                    web3tx(forwarder.execute, "forwarder.execute")(
                        {
                            from: alice,
                            to: superfluid.address,
                            value: "0",
                            gas: "5000000",
                            data: superfluid.contract.methods
                                .forwardBatchCall([])
                                .encodeABI(),
                        },
                        {from: admin}
                    ),
                    "Not trusted forwarder"
                );
            });

            it("#11.3 forwarder with malformatted message", async () => {
                await expectRevertedWith(
                    web3tx(superfluid.forwardBatchCall, "forwarder.execute")(
                        [],
                        {from: admin}
                    ),
                    "Not trusted forwarder"
                );
            });
        });

        describe("#20 Governance", () => {
            it("#20.1 getGovernance", async () => {
                assert.equal(
                    await superfluid.getGovernance.call(),
                    t.contracts.governance.address
                );
            });

            it("#20.2 only governance can replace itself", async () => {
                await expectRevertedWith(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevertedWith(
                    superfluid.replaceGovernance(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#20.3 replace with new governance", async () => {
                const newGov = await TestGovernance.new();
                await web3tx(
                    governance.replaceGovernance,
                    "governance.replaceGovernance"
                )(superfluid.address, newGov.address);
                assert.equal(
                    await superfluid.getGovernance.call(),
                    newGov.address
                );
            });
        });
    });

    context("Non-upgradable deployment", () => {
        let governance;
        let superfluid;

        before(async () => {
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 0,
                tokens: [],
            });

            await t.deployFramework({
                isTruffle: true,
                useMocks: true,
                nonUpgradable: true,
            });
            await t.pushEvmSnapshot();

            // load test suite again after new evm snapshot is created
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 3,
                tokens: [],
            });

            ({admin, alice, bob} = t.aliases);
            ({superfluid, governance} = t.contracts);
        });

        after(async function () {
            await t.popEvmSnapshot();
        });

        describe("#30 non-upgradability", () => {
            it("#30.1 agreement is not upgradable", async () => {
                await expectRevertedWith(
                    governance.updateContracts(
                        superfluid.address,
                        ZERO_ADDRESS,
                        [t.contracts.ida.address],
                        ZERO_ADDRESS
                    ),
                    "SF: non upgradable"
                );
            });

            it("#30.2 supertoken factory logic contract identical and not upgradable", async () => {
                assert.equal(
                    await superfluid.getSuperTokenFactory(),
                    await superfluid.getSuperTokenFactoryLogic()
                );
                const helper = await SuperTokenFactoryHelper.new();
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address,
                    helper.address
                );
                await expectRevertedWith(
                    governance.updateContracts(
                        superfluid.address,
                        ZERO_ADDRESS,
                        [],
                        factory2Logic.address
                    ),
                    "SF: non upgradable"
                );
            });

            it("#30.3 host is not upgradable", async () => {
                const mock1 = await SuperfluidMock.new(
                    false /* nonUpgradable */,
                    false /* appWhiteListingEnabled */
                );
                await expectRevertedWith(
                    governance.updateContracts(
                        superfluid.address,
                        mock1.address,
                        [],
                        ZERO_ADDRESS
                    ),
                    "SF: non upgradable"
                );
            });
        });
    });

    context("App whitelisting deployment", () => {
        const SuperAppMockWithRegistrationkey = artifacts.require(
            "SuperAppMockWithRegistrationkey"
        );

        let superfluid;
        let governance;

        before(async () => {
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 0,
                tokens: [],
            });

            await t.deployFramework({
                isTruffle: true,
                useMocks: true,
                appWhiteListing: true,
            });
            await t.pushEvmSnapshot();

            // load test suite again after new evm snapshot is created
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 3,
                tokens: [],
            });

            ({admin, alice, bob} = t.aliases);
            ({superfluid, governance} = t.contracts);
        });

        after(async function () {
            await t.popEvmSnapshot();
        });

        function createAppKey(deployer, registrationKey) {
            return web3.utils.sha3(
                web3.eth.abi.encodeParameters(
                    ["string", "address", "string"],
                    [
                        "org.superfluid-finance.superfluid.appWhiteListing.registrationKey",
                        deployer,
                        registrationKey,
                    ]
                )
            );
        }

        context("#40.x register app with key", () => {
            it("#40.1 app registration without key should fail", async () => {
                await expectRevertedWith(
                    SuperAppMock.new(
                        superfluid.address,
                        1 /* APP_TYPE_FINAL_LEVEL */,
                        false
                    ),
                    "SF: invalid or expired registration key"
                );
            });

            it("#40.2 app registration with invalid key should fail", async () => {
                await expectRevertedWith(
                    SuperAppMockWithRegistrationkey.new(
                        superfluid.address,
                        1 /* APP_TYPE_FINAL_LEVEL */,
                        "bad microsoft registration key"
                    ),
                    "SF: invalid or expired registration key"
                );
            });

            it("#40.3 app can register with a correct key", async () => {
                const appKey = createAppKey(bob, "hello world");
                const expirationTs =
                    Math.floor(Date.now() / 1000) + 3600 * 24 * 90; // 90 days from now
                await governance.setConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    appKey,
                    expirationTs
                );
                const app = await SuperAppMockWithRegistrationkey.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    "hello world",
                    {
                        from: bob,
                    }
                );
                assert.isTrue(await superfluid.isApp(app.address));
            });

            it("#40.4 app registration with key for different deployer should fail", async () => {
                const appKey = createAppKey(bob, "hello world");
                const expirationTs =
                    Math.floor(Date.now() / 1000) + 3600 * 24 * 90; // 90 days from now
                await governance.setConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    appKey,
                    expirationTs
                );
                await expectRevertedWith(
                    SuperAppMockWithRegistrationkey.new(
                        superfluid.address,
                        1 /* APP_TYPE_FINAL_LEVEL */,
                        "hello world",
                        {
                            from: alice,
                        }
                    ),
                    "SF: invalid or expired registration key"
                );
            });

            it("#40.5 app can register with an expired key should fail", async () => {
                const appKey = createAppKey(bob, "hello world again");
                const expirationTs = Math.floor(Date.now() / 1000) - 3600 * 24; // expired yesterday
                await governance.setConfig(
                    superfluid.address,
                    ZERO_ADDRESS,
                    appKey,
                    expirationTs
                );
                await expectRevertedWith(
                    SuperAppMockWithRegistrationkey.new(
                        superfluid.address,
                        1 /* APP_TYPE_FINAL_LEVEL */,
                        "hello world again",
                        {
                            from: bob,
                        }
                    ),
                    "SF: invalid or expired registration key"
                );
            });
        });

        context("#41.x register app by factory", () => {
            it("#41.1 app registration by unauthorized factory should fail", async () => {
                const appFactory = await SuperAppFactoryMock.new();
                // governance.authorizeAppFactory NOT done
                const app = await SuperAppMockNotSelfRegistering.new();
                await expectRevertedWith(
                    appFactory.registerAppWithHost(
                        superfluid.address,
                        app.address,
                        1 /* APP_TYPE_FINAL_LEVEL */
                    ),
                    "SF: authorized factory required"
                );
            });

            it("#41.2 app registration by authorized factory", async () => {
                const appFactory = await SuperAppFactoryMock.new();
                await governance.authorizeAppFactory(
                    superfluid.address,
                    appFactory.address
                );
                const app = await SuperAppMockNotSelfRegistering.new();
                assert.isFalse(await superfluid.isApp(app.address));
                await appFactory.registerAppWithHost(
                    superfluid.address,
                    app.address,
                    1 /* APP_TYPE_FINAL_LEVEL */
                );
                assert.isTrue(await superfluid.isApp(app.address));

                // works for more than once app...
                const app2 = await SuperAppMockNotSelfRegistering.new();
                assert.isFalse(await superfluid.isApp(app2.address));
                await appFactory.registerAppWithHost(
                    superfluid.address,
                    app2.address,
                    1 /* APP_TYPE_FINAL_LEVEL */
                );
                assert.isTrue(await superfluid.isApp(app2.address));

                // withdrawal of authorization disallows further apps to be registered ...
                await governance.unauthorizeAppFactory(
                    superfluid.address,
                    appFactory.address
                );

                const app3 = await SuperAppMockNotSelfRegistering.new();
                assert.isFalse(await superfluid.isApp(app3.address));
                await expectRevertedWith(
                    appFactory.registerAppWithHost(
                        superfluid.address,
                        app3.address,
                        1 /* APP_TYPE_FINAL_LEVEL */
                    ),
                    "SF: authorized factory required"
                );
            });
        });

        context("#42.x (deprecated) register app", () => {
            it("#42.1 app registration without key should fail", async () => {
                await expectRevertedWith(
                    SuperAppMockUsingDeprecatedRegisterApp.new(
                        superfluid.address,
                        1 /* APP_TYPE_FINAL_LEVEL */
                    ),
                    "SF: app registration requires permission"
                );
            });
        });
    });
});
