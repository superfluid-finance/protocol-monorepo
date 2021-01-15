const { expectRevert, expectEvent } = require("@openzeppelin/test-helpers");

const SuperfluidMock = artifacts.require("SuperfluidMock");
const AgreementMock = artifacts.require("AgreementMock");
const SuperAppMock = artifacts.require("SuperAppMock");
const TestGovernance = artifacts.require("TestGovernance");
const SuperTokenFactory = artifacts.require("SuperTokenFactory");

const TestEnvironment = require("../../TestEnvironment");

const { web3tx, toWad, toBN } = require("@decentral.ee/web3-helpers");

contract("Superfluid Host Contract", accounts => {
    const t = new TestEnvironment(accounts.slice(0, 3), {
        isTruffle: true,
        useMocks: true
    });
    const { admin, alice, bob } = t.aliases;
    const { MAX_UINT256, ZERO_ADDRESS } = t.constants;

    context("Upgradable deployment", () => {
        let governance;
        let superfluid;

        async function reset() {
            await t.reset();
            ({ governance, superfluid } = t.contracts);
        }

        before(async () => {
            await reset();
        });

        describe("#1 upgradability", () => {
            it("#1.1 storage layout", async () => {
                await superfluid.validateStorageLayout.call();
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
                await expectRevert(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#1.4 only can be initialized once", async () => {
                await expectRevert(
                    superfluid.initialize(ZERO_ADDRESS),
                    "Initializable: contract is already initialized"
                );
            });

            it("#1.5 update the code by governanc3", async () => {
                const mock1 = await SuperfluidMock.new(
                    false /* nonUpgradable */
                );
                const mock2 = await SuperfluidMock.new(
                    true /* nonUpgradable */
                );
                await governance.updateContracts(
                    superfluid.address,
                    mock1.address,
                    [],
                    ZERO_ADDRESS
                );
                assert.equal(await superfluid.getCodeAddress(), mock1.address);
                await expectRevert(
                    governance.updateContracts(
                        superfluid.address,
                        mock2.address,
                        [],
                        ZERO_ADDRESS
                    ),
                    "SF: cannot downgrade to non upgradable"
                );
            });
        });

        describe("#2 Agreement Whitelisting", async () => {
            it("#2.1 Agreement whitelisting operations", async () => {
                const N_DEFAULT_AGREEMENTS = (
                    await superfluid.mapAgreementClasses.call(MAX_UINT256)
                ).length;
                const typeA = web3.utils.sha3("typeA");
                const typeB = web3.utils.sha3("typeB");
                const mockA = await AgreementMock.new(typeA, 1);
                const mockAFake = await AgreementMock.new(typeA, 42);
                const mockB = await AgreementMock.new(typeB, 1);
                const mockA2 = await AgreementMock.new(typeA, 2);

                assert.isFalse(
                    await superfluid.isAgreementTypeListed.call(typeA)
                );
                assert.isFalse(
                    await superfluid.isAgreementTypeListed.call(typeB)
                );
                assert.equal(await mockA.agreementType.call(), typeA);

                // register typeA
                await web3tx(
                    governance.registerAgreementClass,
                    "registerAgreementClass typeA"
                )(superfluid.address, mockA.address);
                console.log(
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
                console.log(
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
                await web3tx(
                    governance.updateContracts,
                    "registerAgreementClass typeA"
                )(
                    superfluid.address,
                    ZERO_ADDRESS,
                    [mockA2.address],
                    ZERO_ADDRESS
                );
                console.log(
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
                    toBN(1)
                        .shln(N_DEFAULT_AGREEMENTS)
                        .toString()
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

                await reset();
            });

            it("#2.2 only governance can update agreement listings", async () => {
                await expectRevert(
                    superfluid.registerAgreementClass(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevert(
                    superfluid.updateAgreementClass(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#2.3 only host can update agreement code", async () => {
                await expectRevert(
                    t.contracts.ida.updateCode(ZERO_ADDRESS),
                    "only host can update code"
                );
            });

            it("#2.4 agreement cannot be registered twice", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await AgreementMock.new(typeA, 1);
                const mockA2 = await AgreementMock.new(typeA, 2);

                await web3tx(
                    governance.registerAgreementClass,
                    "registerAgreementClass typeA"
                )(superfluid.address, mockA.address);
                await expectRevert(
                    governance.registerAgreementClass(
                        superfluid.address,
                        mockA2.address
                    ),
                    "SF: agreement class already registered"
                );

                await reset();
            });

            it("#2.5 cannot register more than 256 agreements", async () => {
                const mocks = [];
                mocks.push(t.contracts.cfa.address);
                mocks.push(t.contracts.ida.address);
                for (let i = 0; i < 254; ++i) {
                    process.stdout.write(".");
                    const typeN = web3.utils.sha3("type." + i);
                    const mock = await AgreementMock.new(typeN, 1);
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

                const badMock = await AgreementMock.new(
                    web3.utils.sha3("type.bad"),
                    1
                );
                await expectRevert(
                    governance.registerAgreementClass(
                        superfluid.address,
                        badMock.address
                    ),
                    "SF: support up to 256 agreement classes"
                );

                await reset();
            });

            it("#2.6 agreement must be registered first", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await AgreementMock.new(typeA, 1);

                await expectRevert(
                    governance.updateContracts(
                        superfluid.address,
                        ZERO_ADDRESS,
                        [mockA.address],
                        ZERO_ADDRESS
                    ),
                    "SF: agreement class not registered"
                );

                await expectRevert(
                    superfluid.getAgreementClass(typeA),
                    "SF: agreement class not registered"
                );
                await expectRevert(
                    superfluid.addToAgreementClassesBitmap(0, typeA),
                    "SF: agreement class not registered"
                );
                await expectRevert(
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
                await expectRevert(
                    superfluid.updateSuperTokenFactory(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevert(
                    superfluid.updateSuperTokenLogic(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#3.2 update super token factory", async () => {
                const factory = await superfluid.getSuperTokenFactory();
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address
                );
                await web3tx(
                    governance.updateContracts,
                    "governance.updateContracts"
                )(superfluid.address, ZERO_ADDRESS, [], factory2Logic.address);
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
                await reset();
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
            });

            it("#4.2 app registration only in constructor", async () => {
                await expectRevert(
                    app.tryRegisterApp(0),
                    "SF: app registration only in constructor"
                );
            });

            it("#4.3 app registration with bad config", async () => {
                const reason = "SF: invalid config word";
                await expectRevert(
                    SuperAppMock.new(superfluid.address, 0, false),
                    reason
                );
                await expectRevert(
                    SuperAppMock.new(
                        superfluid.address,
                        1 | (1 << 15) /* jail bit */,
                        false
                    ),
                    reason
                );
            });

            it("#4.4 app double registration should fail", async () => {
                await expectRevert(
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
                await expectRevert(
                    superfluid.allowCompositeApp(app.address),
                    "SF: sender is not an app"
                );
                await expectRevert(
                    app.allowCompositeApp(alice),
                    "SF: target is not an app"
                );
                assert.isFalse(
                    await superfluid.isCompositeAppAllowed.call(
                        app.address,
                        app2.address
                    )
                );
                await web3tx(
                    app2.allowCompositeApp,
                    "app2.allowCompositeApp(app)"
                )(app.address);
                assert.isTrue(
                    await superfluid.isCompositeAppAllowed.call(
                        app2.address,
                        app.address
                    )
                );
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
                        "0x" /* cbdata */
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
                        "0xbeef" /* cbdata */
                    ],
                    "0x" + "faec".repeat(20)
                );

                // error case
                await expectRevert(
                    superfluid.testCtxFuncX(
                        superfluid.contract.methods
                            .ctxFunc1(42, "0xbad")
                            .encodeABI(),
                        "0xbeef"
                    ),
                    "SF: placerholder ctx should have zero length"
                );
            });
        });

        describe("#6 (WIP) Agreement Framework", () => {
            let agreement;
            let app;
            let gasLimit;

            before(async () => {
                gasLimit = (
                    await superfluid.CALLBACK_GAS_LIMIT.call()
                ).toString();
                agreement = await AgreementMock.new(
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
            });

            after(async () => {
                await reset();
            });

            beforeEach(async () => {
                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );
            });

            it("#6.1 only agreement can call the agreement framework", async () => {
                const reason = "SF: sender is not listed agreeement";

                // call from an EOA
                await expectRevert.unspecified(
                    superfluid.callAppBeforeCallback(
                        ZERO_ADDRESS,
                        "0x",
                        false,
                        "0x"
                    )
                );
                await expectRevert.unspecified(
                    superfluid.callAppAfterCallback(
                        ZERO_ADDRESS,
                        "0x",
                        false,
                        "0x"
                    )
                );
                await expectRevert.unspecified(
                    superfluid.appCallbackPush("0x", ZERO_ADDRESS, 0, 0)
                );
                await expectRevert.unspecified(
                    superfluid.appCallbackPop("0x", 0)
                );
                await expectRevert.unspecified(
                    superfluid.ctxUseAllowance("0x", 0, 0)
                );

                // call from an unregisterred mock agreement
                let mock = await AgreementMock.new(web3.utils.sha3("typeA"), 0);
                await expectRevert(
                    mock.tryCallAppBeforeCallback(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryCallAppAfterCallback(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryAppCallbackPush(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryAppCallbackPop(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryCtxUseAllowance(superfluid.address),
                    reason
                );
                await expectRevert(mock.tryJailApp(superfluid.address), reason);

                // call from an in personating mock agreement
                mock = await AgreementMock.new(
                    await t.contracts.cfa.agreementType.call(),
                    0
                );
                await expectRevert(
                    mock.tryCallAppBeforeCallback(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryCallAppAfterCallback(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryAppCallbackPush(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryAppCallbackPop(superfluid.address),
                    reason
                );
                await expectRevert(
                    mock.tryCtxUseAllowance(superfluid.address),
                    reason
                );
                await expectRevert(mock.tryJailApp(superfluid.address), reason);
            });

            // TODO decode ctx
            it("#6.2 beforeAgreementCreated callback noop", async () => {
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
                        cbdata: "0x" + Buffer.from("Noop").toString("hex")
                    }
                );
            });

            it("#6.3 beforeAgreementCreated callback assert or revert", async () => {
                await app.setNextCallbackAction(1 /* assert */, "0x");
                await expectRevert(
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
                    "CallUtils: target reverted"
                );

                await app.setNextCallbackAction(2 /* revert */, "0x");
                await expectRevert(
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
                    "CallUtils: target reverted"
                );

                await app.setNextCallbackAction(
                    3 /* revert with reason */,
                    web3.eth.abi.encodeParameter("string", "error 42")
                );
                await expectRevert(
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

            // TODO decode ctx
            it("#6.4 afterAgreementCreated callback noop", async () => {
                await app.setNextCallbackAction(0 /* noop */, "0x");
                const tx = await superfluid.callAgreement(
                    agreement.address,
                    agreement.contract.methods
                        .callAppAfterAgreementCreatedCallback(app.address, "0x")
                        .encodeABI(),
                    "0x"
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent"
                );
            });

            it("#6.5 afterAgreementCreated callback assert or revert", async () => {
                await app.setNextCallbackAction(1 /* assert */, "0x");
                await expectRevert(
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
                    "CallUtils: target reverted"
                );

                await app.setNextCallbackAction(2 /* revert */, "0x");
                await expectRevert(
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
                    "CallUtils: target reverted"
                );

                await app.setNextCallbackAction(
                    3 /* revert with reason */,
                    web3.eth.abi.encodeParameter("string", "error 42")
                );
                await expectRevert(
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

            it("#6.6 afterAgreementCreated callback altering ctx", async () => {
                await app.setNextCallbackAction(4 /* AlteringCtx */, "0x");
                await expectRevert(
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

            it("#6.7 beforeAgreementTerminated callback revert jail rule", async () => {
                await app.setNextCallbackAction(1 /* assert */, "0x");
                const tx = await web3tx(
                    superfluid.callAgreement,
                    "callAgreement"
                )(
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
                        reason: "10" // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                    }
                );
            });

            it("#6.8 afterAgreementTerminated callback revert jail rule", async () => {
                await app.setNextCallbackAction(1 /* assert */, "0x");
                const tx = await web3tx(
                    superfluid.callAgreement,
                    "callAgreement"
                )(
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
                        reason: "10" // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                    }
                );
            });

            it("#6.9 afterAgreementTerminated callback readonly ctx jail rule", async () => {
                await app.setNextCallbackAction(4 /* AlteringCtx */, "0x");
                const tx = await web3tx(
                    superfluid.callAgreement,
                    "callAgreement"
                )(
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
                        reason: "20" // APP_RULE_CTX_IS_READONLY
                    }
                );
            });

            it("#6.11 callback will not be called for jailed apps", async () => {
                await superfluid.jailApp(app.address);
                await app.setNextCallbackAction(1 /* assert */, "0x");
                await web3tx(superfluid.callAgreement, "callAgreement")(
                    agreement.address,
                    agreement.contract.methods
                        .callAppAfterAgreementCreatedCallback(app.address, "0x")
                        .encodeABI(),
                    "0x"
                );
            });

            it("#6.12 testIsValidAbiEncodedBytes", async () => {
                await superfluid.testIsValidAbiEncodedBytes();
            });

            describe("#6.2x callback gas limit", () => {
                it("#6.20 beforeCreated callback burn all gas", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // burn all the gas
                    await expectRevert(
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
                        "CallUtils: target reverted"
                    );
                });

                it("#6.21 beforeCreated callback try to burn all gas but less gas provided", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // provide less gas
                    await expectRevert(
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
                                gas: Math.ceil(gasLimit / 2)
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
                        "0x"
                    );
                    assert.isTrue(await superfluid.isAppJailed(app.address));
                    await expectEvent.inTransaction(
                        tx.tx,
                        superfluid.contract,
                        "Jail",
                        {
                            app: app.address,
                            reason: "10" // APP_RULE_NO_REVERT_ON_TERMINATION_CALLBACK
                        }
                    );
                });

                it("#6.23 afterTerminated try to burn all gas but with less gas provided", async () => {
                    await app.setNextCallbackAction(
                        5 /* BurnGas */,
                        web3.eth.abi.encodeParameter("uint256", gasLimit)
                    );

                    // provide less gas
                    await expectRevert(
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
                                gas: Math.ceil(gasLimit / 2)
                            }
                        ),
                        "SF: need more gas"
                    );
                });

                it("#6.24 beforeCreated try to burn just enough gas", async () => {
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
                    console.log("Gas used", tx.receipt.gasUsed);
                    let gasLowerBound = tx.receipt.gasUsed;
                    let gasUpperBound = gasLowerBound + 300000;
                    console.log("Current bound", gasLowerBound, gasUpperBound);

                    // binary search proof if there is a price can trigger unexpected revert
                    let gas;
                    let errorCount = 0;
                    let successCount = 0;
                    while (gasLowerBound <= gasUpperBound) {
                        const gap = Math.floor(
                            (gasUpperBound - gasLowerBound) / 2
                        );
                        gas = gasLowerBound + gap;
                        console.log("Trying with new gas limit", gas);
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
                                    gas
                                }
                            );
                            console.log("Gas used", tx.receipt.gasUsed);
                            console.log(
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
                            console.log(
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

            describe("#6.3x composite app rules", () => {
                const SuperAppMock3 = artifacts.require("SuperAppMock3");

                it("#6.30 composite app must be whitelisted", async () => {
                    // assuming MAX_APP_LEVEL = 1
                    const app3 = await SuperAppMock3.new(
                        superfluid.address,
                        app.address,
                        agreement.address
                    );
                    await expectRevert(
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
                    await web3tx(
                        app3.allowCompositeApp,
                        "app3.allowCompositeApp(app)"
                    )();
                    await expectRevert(
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
                    const app3 = await SuperAppMock3.new(
                        superfluid.address,
                        app.address,
                        agreement.address
                    );
                    await superfluid.jailApp(app.address);
                    await expectRevert(
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
                        "SF: APP_RULE_COMPOSITE_APP_IS_JAILED"
                    );
                });
            });

            describe("#6.4x invalid ctx returned by the callback", () => {
                const SuperAppMock2 = artifacts.require(
                    "SuperAppMockReturningEmptyCtx"
                );
                const SuperAppMock3 = artifacts.require(
                    "SuperAppMockReturningInvalidCtx"
                );

                it("#6.40 should give explicit error message in non-termination callbacks", async () => {
                    const app2 = await SuperAppMock2.new(superfluid.address);
                    await expectRevert(
                        web3tx(
                            superfluid.callAgreement,
                            "to SuperAppMockReturningEmptyCtx"
                        )(
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

                    const app3 = await SuperAppMock3.new(superfluid.address);
                    await expectRevert(
                        web3tx(
                            superfluid.callAgreement,
                            "to SuperAppMockReturningInvalidCtx"
                        )(
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
                    const app2 = await SuperAppMock2.new(superfluid.address);
                    let tx = await web3tx(
                        superfluid.callAgreement,
                        "callAgreement"
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
                            reason: "22" // APP_RULE_CTX_IS_EMPTY
                        }
                    );

                    const app3 = await SuperAppMock3.new(superfluid.address);
                    tx = await web3tx(
                        superfluid.callAgreement,
                        "callAgreement"
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
                            reason: "22" // APP_RULE_CTX_IS_EMPTY
                        }
                    );
                });
            });

            // TODO app allowance
            // TODO app callback masks
        });

        describe("#7 callAgreement", () => {
            it("#7.1 only listed agreement allowed", async () => {
                const reason = "SF: only listed agreeement allowed";
                // call to an non agreement
                await expectRevert.unspecified(
                    superfluid.callAgreement(alice, "0x", "0x")
                );
                // call to an unregisterred mock agreement
                let mock = await AgreementMock.new(web3.utils.sha3("typeA"), 0);
                await expectRevert(
                    superfluid.callAgreement(mock.address, "0x", "0x"),
                    reason
                );
                // call to an in personating mock agreement
                mock = await AgreementMock.new(
                    await t.contracts.cfa.agreementType.call(),
                    0
                );
                await expectRevert(
                    superfluid.callAgreement(mock.address, "0x", "0x"),
                    reason
                );
            });

            it("#7.2 callData without correct selector", async () => {
                await expectRevert(
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
                agreement = await AgreementMock.new(
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
            });

            after(async () => {
                await reset();
            });

            beforeEach(async () => {
                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );
            });

            it("#8.1 only super app can be called", async () => {
                const reason = "SF: not a super app";
                // call to an non agreement
                await expectRevert.unspecified(
                    superfluid.callAppAction(alice, "0x")
                );
                // call to an unregisterred mock agreement
                await expectRevert(
                    superfluid.callAppAction(governance.address, "0x"),
                    reason
                );
            });

            // TODO decode ctx
            it("#8.2 actionNoop", async () => {
                const tx = await superfluid.callAppAction(
                    app.address,
                    app.contract.methods.actionNoop("0x").encodeABI()
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent"
                );
            });

            it("#8.3 callAppAction assert or revert", async () => {
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionAssert("0x").encodeABI()
                    ),
                    "CallUtils: target reverted"
                );
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionRevert("0x").encodeABI()
                    ),
                    "CallUtils: target reverted"
                );
                await expectRevert(
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
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallAgreementWithoutCtx("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_NOT_CLEAN"
                );
                await expectRevert(
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
                        ping: "42"
                    }
                );
            });

            it("#8.6 app callAgreementWithContext which reverts", async () => {
                await expectRevert(
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
                await expectRevert(
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
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods.actionAlteringCtx("0x").encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_READONLY"
                );
            });

            it("#8.10 should not be able call jailed app", async () => {
                await superfluid.jailApp(app.address);
                await expectRevert(
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
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionReturnEmptyCtx("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_EMPTY"
                );
            });
        });

        describe("#9 Contextual Call Proxies", () => {
            let agreement;
            let app;

            before(async () => {
                agreement = await AgreementMock.new(
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
            });

            after(async () => {
                await reset();
            });

            beforeEach(async () => {
                app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );
            });

            it("#9.1 must call with valid ctx", async () => {
                await expectRevert(
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
                await expectRevert(
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
                await expectRevert(
                    superfluid.callAppAction(
                        app.address,
                        app.contract.methods
                            .actionCallBadAction("0x")
                            .encodeABI()
                    ),
                    "SF: APP_RULE_CTX_IS_READONLY"
                );
            });
        });

        describe("#10 batchCall", () => {
            it("#10.1 batchCall upgrade/approve/transfer/downgrade in one", async () => {
                await t.createNewToken({ doUpgrade: false });
                const { superToken } = t.contracts;

                await web3tx(superToken.upgrade, "Alice upgrades 10 tokens")(
                    toWad("10"),
                    {
                        from: alice
                    }
                );

                await web3tx(
                    superToken.approve,
                    "SuperToken.approve - from alice to admin"
                )(admin, toWad("3"), {
                    from: alice
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
                            )
                        ],
                        [
                            1, // approve
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "uint256"],
                                [bob, toWad("1").toString()]
                            )
                        ],
                        [
                            2, // transferFrom own funds
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "address", "uint256"],
                                [admin, bob, toWad("2").toString()]
                            )
                        ],
                        [
                            2, // transferFrom other's funds
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["address", "address", "uint256"],
                                [alice, bob, toWad("3").toString()]
                            )
                        ],
                        [
                            102, // downgrade
                            superToken.address,
                            web3.eth.abi.encodeParameters(
                                ["uint256"],
                                [toWad("5").toString()]
                            )
                        ]
                    ],
                    {
                        from: admin
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
                let agreement = await AgreementMock.new(
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
                                    "0x" // user data
                                ]
                            )
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
                                    "0x" // user data
                                ]
                            )
                        ]
                    ],
                    {
                        from: admin
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "42"
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "43"
                    }
                );

                await reset();
            });

            it("#10.3 batchCall call app action", async () => {
                let agreement = await AgreementMock.new(
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
                                    "0x" // user data
                                ]
                            )
                        ],
                        [
                            202, // call app action
                            app.address,
                            app.contract.methods
                                .actionExpectMsgSender(admin, "0x")
                                .encodeABI()
                        ]
                    ],
                    {
                        from: admin
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    agreement.contract,
                    "Pong",
                    {
                        ping: "42"
                    }
                );
                await expectEvent.inTransaction(
                    tx.tx,
                    app.contract,
                    "NoopEvent"
                );

                await reset();
            });

            it("#10.4 batchCall one fail revert all", async () => {
                const app = await SuperAppMock.new(
                    superfluid.address,
                    1 /* APP_TYPE_FINAL_LEVEL */,
                    false
                );

                await expectRevert(
                    web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                        [
                            [
                                202, // call app action
                                app.address,
                                app.contract.methods
                                    .actionCallActionNoop("0x")
                                    .encodeABI()
                            ],
                            [
                                202, // call app action
                                app.address,
                                app.contract.methods
                                    .actionCallActionRevert("error 42", "0x")
                                    .encodeABI()
                            ]
                        ],
                        {
                            from: admin
                        }
                    ),
                    "error 42"
                );
            });

            it("#10.5 batchCall invalid operation type", async () => {
                await expectRevert(
                    web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                        [[8888, ZERO_ADDRESS, "0x"]],
                        {
                            from: admin
                        }
                    ),
                    "SF: unknown batch call operation type"
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
                await expectRevert(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
                await expectRevert(
                    superfluid.replaceGovernance(ZERO_ADDRESS),
                    "SF: only governance allowed"
                );
            });

            it("#20.3 replace with new governance", async () => {
                const newGov = await TestGovernance.new(ZERO_ADDRESS, 1000);
                await web3tx(
                    governance.replaceGovernance,
                    "superfluid.replaceGovernance"
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
            await t.reset({ nonUpgradable: true });
            ({ governance, superfluid } = t.contracts);
        });

        describe("#30 non-upgradability", () => {
            it("#30.1 agreement is not upgradable", async () => {
                await expectRevert(
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
                const factory2Logic = await SuperTokenFactory.new(
                    superfluid.address
                );
                await expectRevert(
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
                    false /* nonUpgradable */
                );
                await expectRevert(
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
});
