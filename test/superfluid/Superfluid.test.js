const { expectRevert } = require("@openzeppelin/test-helpers");

const TestToken = artifacts.require("TestToken");
const AgreementMock = artifacts.require("AgreementMock");

const Tester = require("../superfluid/Tester");

const {
    web3tx,
    toWad,
    toBN
} = require("@decentral.ee/web3-helpers");

contract("Superfluid", accounts => {

    const tester = new Tester(accounts.slice(0, 3));
    const { admin, alice, bob } = tester.aliases;
    const { MAX_UINT256 } = tester.constants;

    let governance;
    let superfluid;
    let superToken;

    before(async () => {
        tester.printAliases();
    });

    beforeEach(async function () {
        await tester.resetContracts();
        ({
            governance,
            superfluid,
            superToken
        } = tester.contracts);
    });

    describe("#1 Agreement Whitelisting", async () => {

        it("#1.1 Agreement whitelisting operations", async () => {
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
            await expectRevert(
                governance.registerAgreementClass(superfluid.address, mockA2.address),
                "SF: Agreement class already registered");
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
        });

    });

    describe("#2 Token Registry", () => {

        it("#2.1 ERC20Wrapper", async () => {

            const token1 = await web3tx(TestToken.new, "TestToken.new 1")("Test Token 1", "TT1");
            const token2 = await web3tx(TestToken.new, "TestToken.new 2")("Test Token 2", "TT2");
            const result1 = await superfluid.getERC20Wrapper.call(
                token1.address,
                "TEST1x",
            );
            assert.isFalse(result1.created);
            const result2 = await superfluid.getERC20Wrapper.call(
                token2.address,
                "TEST2x",
            );
            assert.notEqual(result1.wrapperAddress, result2.wrapperAddress);
            assert.isFalse(result2.created);
            await web3tx(superfluid.createERC20Wrapper, "registry.createERC20Wrapper 1")(
                token1.address,
                18,
                "Super Test Token 1",
                "TEST1x", {
                    from: admin
                }
            );
            const result1b = await superfluid.getERC20Wrapper.call(
                token1.address,
                "TEST1x"
            );
            assert.isTrue(result1b.created);
            assert.equal(result1.wrapperAddress, result1b.wrapperAddress);
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
        // 5.1 callAgreement
        // 5.2 callAppAction
        it("#5.30 batchCall upgrade/approve/transfer/downgrade in 1", async () => {
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

            await tester.validateSystem();
        });
    });

    describe("#6 Contextual Call Proxy", async () => {
        // TODO
        // callAgreementWithContext
        // callAppActionWithContext
        // chargeGasFee ?
    });

    describe("#10 Governance", () => {
        it("#10.1 getGovernance", async () => {
            assert.equal(
                await superfluid.getGovernance.call(),
                tester.contracts.governance.address);
        });
    });

});
