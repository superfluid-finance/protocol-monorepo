const {
    expectRevert
    // expectEvent
} = require("@openzeppelin/test-helpers");

const {
    web3tx,
    //toBN
    // toDecimals,
    // toBN
} = require("@decentral.ee/web3-helpers");

const TestEnvironment = require("../../TestEnvironment");
const AgreementMock = artifacts.require("AgreementMock");

contract("SuperfluidToken implementation", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 3));
    const { alice, bob } = t.aliases;
    const { ZERO_BYTES32 } = t.constants;

    // let token;
    let superToken;
    let superfluid;
    let governance;

    before(async () => {
        await t.reset();
        ({
            superfluid,
            governance
        } = t.contracts);
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({
            superToken,
        } = t.contracts);
    });

    describe("#1 basic information", () => {
        it("#1.1 should return host", async () => {
            assert.equal(await superToken.getHost.call(), superfluid.address);
        });
    });

    describe("#2 agreement hosting functions", () => {
        let acA; // agreement class A
        const testData = [
            "0xdead000000000000000000000000000000000000000000000000000000000000",
            "0x000000000000000000000000000000000000000000000000000000000000beaf",
        ];
        const testData2 = [
            "0x771c362cd8f0f3f5c1ef27d2a79641f3d14131fcfefd59ccac42c13a52831384",
            "0x3887e65223d48ea8470b791ed887db139f831bb98e66a607531b3a7be4977cfb",
        ];

        before(async () => {
            const acALogic = await AgreementMock.new(web3.utils.sha3("typeA"), 1);
            await web3tx(governance.registerAgreementClass, "register agreement class typeA")(
                superfluid.address,
                acALogic.address
            );
            acA = await AgreementMock.at(
                await superfluid.getAgreementClass(web3.utils.sha3("typeA"))
            );
        });

        context("#2.a agreement data", () => {
            it("#2.a.1 should create new agreement", async function () {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x42", 2),
                    testData);
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x42", 1),
                    [testData[0]]);
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x42", 3),
                    [...testData, ZERO_BYTES32]);
            });

            it("#2.a.2 should not create the same agreement twice", async () => {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                await expectRevert(acA.createAgreementFor(
                    superToken.address, "0x42", testData
                ), "SuperfluidToken: agreement already created");
                // try overlapping data
                await expectRevert(acA.createAgreementFor(
                    superToken.address, "0x42", [testData[0]]
                ), "SuperfluidToken: agreement already created");
                await expectRevert(acA.createAgreementFor(
                    superToken.address, "0x42", [...testData, ...testData]
                ), "SuperfluidToken: agreement already created");
            });

            it("#2.a.3 should not overlap data", async () => {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x43", 1),
                    [ZERO_BYTES32]);
            });

            it("#2.a.4 should update data", async () => {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                await acA.updateAgreementDataFor(
                    superToken.address, "0x42", testData2
                );
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x42", 2),
                    testData2);
            });

            it("#2.a.5 should terminate agreement", async () => {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                await acA.terminateAgreementFor(
                    superToken.address, "0x42", 2
                );
                assert.deepEqual(
                    await superToken.getAgreementData(acA.address, "0x42", 2),
                    [ZERO_BYTES32, ZERO_BYTES32]);
            });

            it("#2.a.6 should not terminate agreement twice", async () => {
                await acA.createAgreementFor(
                    superToken.address, "0x42", testData
                );
                await acA.terminateAgreementFor(
                    superToken.address, "0x42", 2
                );
                await expectRevert(acA.terminateAgreementFor(
                    superToken.address, "0x42", 2
                ), "SuperfluidToken: agreement does not exist");
            });
        });

        context("#2.b agreement account state", () => {
            it("#2.b.1 should update agreement state", async () => {
                await acA.updateAgreementStateSlotFor(
                    superToken.address, bob, 42, testData
                );
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 42, 2),
                    testData);
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 42, 1),
                    [testData[0]]);
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 42, 3),
                    [...testData, ZERO_BYTES32]);
                await acA.updateAgreementStateSlotFor(
                    superToken.address, bob, 42, testData2
                );
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 42, 2),
                    testData2);
            });

            it("#2.b.2 should overlap agreement state data", async () => {
                await acA.updateAgreementStateSlotFor(
                    superToken.address, bob, 42, testData
                );
                await acA.updateAgreementStateSlotFor(
                    superToken.address, bob, 43, testData2
                );
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 42, 2),
                    testData);
                assert.deepEqual(
                    await superToken.getAgreementStateSlot(acA.address, bob, 43, 2),
                    testData2);
            });
        });

        context("#2.c static balance", () => {
            it("#2.c.1 should only be called by listed agreement", async () => {
                const acBad = await AgreementMock.new(web3.utils.sha3("typeBad"), 1);
                await expectRevert(
                    acBad.settleBalanceFor(superToken.address, bob, "1"),
                    "SuperfluidToken: only listed agreeement");
            });

            it("#2.c.1 should adjust static balance", async () => {
                const availableBalanceOf = async n => {
                    return (await superToken.realtimeBalanceOfNow(n)).availableBalance;
                };
                assert.equal(await availableBalanceOf(bob), "0");
                await acA.settleBalanceFor(superToken.address, bob, "5");
                assert.equal(await availableBalanceOf(bob), "5");
                await acA.settleBalanceFor(superToken.address, bob, "-10");
                assert.equal(await availableBalanceOf(bob), "-5");
                assert.equal(await availableBalanceOf(alice), "0");
                await acA.settleBalanceFor(superToken.address, alice, "42");
                assert.equal(await availableBalanceOf(alice), "42");
            });
        });

        context("#2.d liquidation rules", () => {
            it("#2.d.1 should only be called by listed agreement", async () => {
                const acBad = await AgreementMock.new(web3.utils.sha3("typeBad"), 1);
                await expectRevert(
                    acBad.liquidateAgreementFor(superToken.address, bob, "0x42", alice, 0, 0),
                    "SuperfluidToken: only listed agreeement");
            });
        });

    });

    describe("#3 real-time balance", () => {
    });

});
