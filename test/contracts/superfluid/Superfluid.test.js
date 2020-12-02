const { expectRevert } = require("@openzeppelin/test-helpers");

const AgreementMock = artifacts.require("AgreementMock");
const TestGovernance = artifacts.require("TestGovernance");

const TestEnvironment = require("../../TestEnvironment");

const {
    web3tx,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");


contract("Superfluid Host Contract", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 3));
    const { admin, alice, bob } = t.aliases;
    const { MAX_UINT256, ZERO_ADDRESS } = t.constants;

    let governance;
    let superfluid;
    let superToken;

    context("Upgradable deployment", () => {

        before(async () => {
            await t.reset();
            ({
                governance,
                superfluid,
            } = t.contracts);
        });

        beforeEach(async function () {
            await t.createNewToken({ doUpgrade: false });
            ({
                superToken
            } = t.contracts);
        });

        describe("#1 upgradability", () => {
            it("#1.1 storage layout", async () => {
                await superfluid.validateStorageLayout.call();
            });

            it("#1.2 proxiable info", async () => {
                assert.equal(await superfluid.proxiableUUID.call(),
                    web3.utils.sha3("org.superfluid-finance.contracts.Superfluid.implementation"));
            });

            it("#1.3 only governance can update the code", async () => {
                await expectRevert(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: Only governance allowed");
            });

            it("#1.4 only can initialize once", async () => {
                await expectRevert(
                    superfluid.initialize(
                        ZERO_ADDRESS,
                    ),
                    "Initializable: contract is already initialized");
            });
        });

        describe("#2 Agreement Whitelisting", async () => {

            it("#2.1 Agreement whitelisting operations", async () => {
                const N_DEFAULT_AGREEMENTS = (await superfluid.mapAgreementClasses.call(MAX_UINT256)).length;
                const typeA = web3.utils.sha3("typeA");
                const typeB = web3.utils.sha3("typeB");
                const mockA = await AgreementMock.new(typeA, 1);
                const mockAFake = await AgreementMock.new(typeA, 42);
                const mockB = await AgreementMock.new(typeB, 1);
                const mockA2 = await AgreementMock.new(typeA, 2);

                assert.isFalse(await superfluid.isAgreementTypeListed.call(typeA));
                assert.isFalse(await superfluid.isAgreementTypeListed.call(typeB));
                assert.equal(await mockA.agreementType.call(), typeA);

                // register typeA
                await web3tx(governance.registerAgreementClass, "registerAgreementClass typeA")(
                    superfluid.address,
                    mockA.address
                );
                console.log("Agreement classes", await superfluid.mapAgreementClasses.call(MAX_UINT256));
                const mockAProxy = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeA));
                assert.equal(await mockAProxy.version.call(), 1);
                assert.equal(await mockA.agreementType.call(), await mockAProxy.agreementType.call());
                assert.isTrue(await superfluid.isAgreementTypeListed.call(typeA));
                assert.isTrue(await superfluid.isAgreementClassListed.call(mockAProxy.address));
                assert.isFalse(await superfluid.isAgreementClassListed.call(mockA.address));
                assert.isFalse(await superfluid.isAgreementClassListed.call(mockAFake.address));
                assert.isFalse(await superfluid.isAgreementTypeListed.call(typeB));
                assert.deepEqual(
                    (await superfluid.mapAgreementClasses.call(MAX_UINT256)).slice(-1),
                    [mockAProxy.address]);

                // register typeB
                await web3tx(governance.registerAgreementClass, "registerAgreementClass typeB")(
                    superfluid.address,
                    mockB.address
                );
                console.log("Agreement classes", await superfluid.mapAgreementClasses.call(MAX_UINT256));
                const mockBProxy = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeB)
                );
                assert.equal(await mockBProxy.version.call(), 1);
                assert.equal(await mockB.agreementType.call(), await mockBProxy.agreementType.call());
                assert.isTrue(await superfluid.isAgreementTypeListed.call(typeA));
                assert.isTrue(await superfluid.isAgreementClassListed.call(mockAProxy.address));
                assert.isTrue(await superfluid.isAgreementTypeListed.call(typeB));
                assert.isTrue(await superfluid.isAgreementClassListed.call(mockBProxy.address));
                assert.deepEqual(
                    (await superfluid.mapAgreementClasses.call(MAX_UINT256)).slice(-2),
                    [mockAProxy.address, mockBProxy.address]);

                // upgrade typeA
                await web3tx(governance.updateAgreementClass, "registerAgreementClass typeA")(
                    superfluid.address,
                    mockA2.address
                );
                console.log("Agreement classes", await superfluid.mapAgreementClasses.call(MAX_UINT256));
                const mockAProxy2 = await AgreementMock.at(
                    await superfluid.getAgreementClass.call(typeA));
                assert.equal(mockAProxy2.address, mockAProxy.address);
                assert.equal(await mockAProxy2.version.call(), 2);

                // bitmap operations
                assert.equal(
                    toBN(MAX_UINT256).xor(
                        toBN(await superfluid.removeFromAgreementClassesBitmap.call(MAX_UINT256, typeA))
                    ).toString(),
                    toBN(1).shln(N_DEFAULT_AGREEMENTS).toString());
                assert.equal(
                    toBN(MAX_UINT256).xor(
                        toBN(await superfluid.removeFromAgreementClassesBitmap.call(MAX_UINT256, typeB))
                    ).toString(),
                    toBN(1).shln(N_DEFAULT_AGREEMENTS+1).toString());
                assert.equal(
                    (await superfluid.addToAgreementClassesBitmap(
                        (await superfluid.removeFromAgreementClassesBitmap.call(MAX_UINT256, typeA)).toString(),
                        typeA
                    )).toString(),
                    MAX_UINT256);

                await t.reset();
                ({
                    governance,
                    superfluid,
                } = t.contracts);
            });

            it("#2.2 only governance can update agreement listings", async () => {
                await expectRevert(
                    superfluid.registerAgreementClass(ZERO_ADDRESS),
                    "SF: Only governance allowed");
                await expectRevert(
                    superfluid.updateAgreementClass(ZERO_ADDRESS),
                    "SF: Only governance allowed");
            });

            it("#2.3 only host can update agreement code", async () => {
                await expectRevert(
                    t.contracts.ida.updateCode(ZERO_ADDRESS),
                    "only host can update code");
            });

            it("#2.4 agreement cannot be registered twice", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await AgreementMock.new(typeA, 1);
                const mockA2 = await AgreementMock.new(typeA, 2);

                await web3tx(governance.registerAgreementClass, "registerAgreementClass typeA")(
                    superfluid.address,
                    mockA.address
                );
                await expectRevert(
                    governance.registerAgreementClass(superfluid.address, mockA2.address),
                    "SF: agreement class already registered");

                await t.reset();
                ({
                    governance,
                    superfluid,
                } = t.contracts);
            });

            it("#2.5 cannot register more than 256 agreements", async () => {
                const mocks = [];
                mocks.push(t.contracts.cfa.address);
                mocks.push(t.contracts.ida.address);
                for (let i = 0; i < 254; ++i) {
                    const typeN = web3.utils.sha3("type."+i);
                    const mock = await AgreementMock.new(typeN, 1);
                    await governance.registerAgreementClass(
                        superfluid.address,
                        mock.address
                    );
                    mocks.push(await superfluid.getAgreementClass(typeN));
                }

                const agreements = await superfluid.mapAgreementClasses.call(MAX_UINT256);
                for (let i = 0; i < 256; ++i) {
                    assert.equal(agreements[i], mocks[i], `agreement no.${i} mismatch`);
                }
                const badMock = await AgreementMock.new(web3.utils.sha3("type.bad"), 1);
                await expectRevert(
                    governance.registerAgreementClass(superfluid.address, badMock.address),
                    "SF: support up to 256 agreement classes");

                await t.reset();
                ({
                    governance,
                    superfluid,
                } = t.contracts);
            });

            it("#2.6 agreement must be registered first", async () => {
                const typeA = web3.utils.sha3("typeA");
                const mockA = await AgreementMock.new(typeA, 1);

                await expectRevert(governance.updateAgreementClass(
                    superfluid.address,
                    mockA.address
                ), "SF: agreement class not registered");

                await expectRevert(superfluid.getAgreementClass(typeA),
                    "SF: agreement class not registered");
                await expectRevert(superfluid.addToAgreementClassesBitmap(0, typeA),
                    "SF: agreement class not registered");
                await expectRevert(superfluid.removeFromAgreementClassesBitmap(0, typeA),
                    "SF: agreement class not registered");
            });

        });

        describe("#3 App Registry", async () => {
            // TODO
        });

        describe("#4 Agreement Callback System", async () => {
            // TODO
        });

        describe("#5 Non-app Call Proxy", async () => {
            // TODO
            // 5.a callAgreement
            // 5.b callAppAction

            // 5.c
            context("#5.a batchCall", () => {
                it("#5.a.1 batchCall upgrade/approve/transfer/downgrade in one", async () => {
                    await web3tx(superToken.upgrade, "Alice upgrades 10 tokens")(
                        toWad("10"), {
                            from: alice
                        }
                    );

                    await web3tx(superToken.approve, "SuperToken.approve - from alice to admin")(
                        admin,
                        toWad("3"), {
                            from: alice
                        }
                    );
                    assert.equal(
                        (await superToken.allowance.call(alice, admin)).toString(),
                        toWad("3").toString());

                    await web3tx(superfluid.batchCall, "Superfluid.batchCall")(
                        [
                            [
                                2, // upgrade
                                superToken.address,
                                web3.eth.abi.encodeParameters(
                                    ["uint256"],
                                    [toWad("10").toString()])
                            ],
                            [
                                0, // approve
                                superToken.address,
                                web3.eth.abi.encodeParameters(
                                    ["address", "uint256"],
                                    [bob, toWad("1").toString()])
                            ],
                            [
                                1, // transferFrom own funds
                                superToken.address,
                                web3.eth.abi.encodeParameters(
                                    ["address", "address", "uint256"],
                                    [admin, bob, toWad("2").toString()])
                            ],
                            [
                                1, // transferFrom other's funds
                                superToken.address,
                                web3.eth.abi.encodeParameters(
                                    ["address", "address", "uint256"],
                                    [alice, bob, toWad("3").toString()])
                            ],
                            [
                                3, // downgrade
                                superToken.address,
                                web3.eth.abi.encodeParameters(["uint256"], [toWad("5").toString()])
                            ]
                        ],
                        {
                            from: admin
                        }
                    );
                    assert.equal(
                        (await superToken.balanceOf.call(admin)).toString(),
                        toWad("3").toString());
                    assert.equal(
                        (await superToken.allowance.call(alice, admin)).toString(),
                        toWad("0").toString());
                    assert.equal(
                        (await superToken.balanceOf.call(alice)).toString(),
                        toWad("7").toString());
                    assert.equal(
                        (await superToken.allowance.call(admin, bob)).toString(),
                        toWad("1").toString());
                    assert.equal(
                        (await superToken.balanceOf.call(bob)).toString(),
                        toWad("5").toString());

                    await t.validateSystemInvariance();
                });
            });
        });

        describe("#6 Contextual Call Proxy", async () => {
            // TODO
            // callAgreementWithContext
            // callAppActionWithContext
            // chargeGasFee ?
        });

        describe("#9 Super token factory", () => {
            it("#9.1 only governance can update", async () => {
                await expectRevert(
                    superfluid.updateSuperTokenFactory(ZERO_ADDRESS),
                    "SF: Only governance allowed");
                await expectRevert(
                    superfluid.updateSuperTokenLogic(ZERO_ADDRESS),
                    "SF: Only governance allowed");
            });
        });

        describe("#10 Governance", () => {
            it("#10.1 getGovernance", async () => {
                assert.equal(
                    await superfluid.getGovernance.call(),
                    t.contracts.governance.address);
            });

            it("#10.2 only governance can replace itself", async () => {
                await expectRevert(
                    superfluid.updateCode(ZERO_ADDRESS),
                    "SF: Only governance allowed");
                await expectRevert(
                    superfluid.replaceGovernance(ZERO_ADDRESS),
                    "SF: Only governance allowed");
            });

            it("#10.3 replace with new governance", async () => {
                const newGov = await TestGovernance.new(
                    ZERO_ADDRESS,
                    1000
                );
                await web3tx(governance.replaceGovernance, "superfluid.replaceGovernance")(
                    superfluid.address,
                    newGov.address
                );
                assert.equal(
                    await superfluid.getGovernance.call(),
                    newGov.address);
            });
        });

    });

    context("Non-upgradable deployment", () => {

        before(async () => {
            await t.reset({ nonUpgradable: true });
            ({
                governance,
                superfluid,
            } = t.contracts);
        });

        describe("#100 non-upgradability", () => {
            it("#100.1 agreement is not upgradable", async () => {
                await expectRevert(
                    governance.updateAgreementClass(
                        superfluid.address,
                        t.contracts.ida.address),
                    "SF: non upgradable");
            });

            it("#100.2 supertoken factory logic contract identical and not upgradable", async () => {
                assert.equal(
                    await superfluid.getSuperTokenFactory(),
                    await superfluid.getSuperTokenFactoryLogic());
                await expectRevert(
                    governance.updateSuperTokenFactory(
                        superfluid.address,
                        ZERO_ADDRESS),
                    "SF: non upgradable");
            });
        });

    });

});
