const {
    expectRevert
    // expectEvent
} = require("@openzeppelin/test-helpers");

// const {
//     //web3tx,
//     toBN
//     // toDecimals,
//     // toBN
// } = require("@decentral.ee/web3-helpers");

const TestEnvironment = require("../../TestEnvironment");
const AgreementMock = artifacts.require("AgreementMock");

contract("SuperfluidToken implementation", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 4));
    const { bob} = t.aliases;
    const { ZERO_BYTES32 } = t.constants;

    // let token;
    let superToken;
    let superfluid;

    before(async () => {
        await t.reset();
        ({
            superfluid,
        } = t.contracts);
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({
            superToken,
        } = t.contracts);
    });

    describe("#1 basic information", function () {
        it("#1.1 should return host", async () => {
            assert.equal(await superToken.getHost.call(), superfluid.address);
        });
    });

    describe("#2 agreement data and state storages", function () {
        let acA; // agreement class A
        const testData = [
            "0xdead000000000000000000000000000000000000000000000000000000000000",
            "0x000000000000000000000000000000000000000000000000000000000000beaf",
        ];
        const testData2 = [
            "0x771c362cd8f0f3f5c1ef27d2a79641f3d14131fcfefd59ccac42c13a52831384",
            "0x3887e65223d48ea8470b791ed887db139f831bb98e66a607531b3a7be4977cfb",
        ];

        beforeEach(async () => {
            acA = await AgreementMock.new(web3.utils.sha3("typeA"), 1);
        });

        it("#2.1 should create new agreement", async function () {
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

        it("#2.2 should not create the same agreement twice", async () => {
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

        it("#2.3 should not overlap data", async () => {
            await acA.createAgreementFor(
                superToken.address, "0x42", testData
            );
            assert.deepEqual(
                await superToken.getAgreementData(acA.address, "0x43", 1),
                [ZERO_BYTES32]);
        });

        it("#2.4 should update data", async () => {
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

        it("#2.5 should terminate agreement", async () => {
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

        it("#2.6 should not terminate agreement twice", async () => {
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

        it("#2.7 should update agreement state", async () => {
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

        it("#2.8 should overlap agreement state data", async () => {
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

    describe("#3 real-time balance", function () {
    });

    // describe("#2 SuperToken ISuperAgreementStorage(TBD) operations", () => {
    //     // TODO To be improved with a mock agreement class
    //
    //     it("#2.1 - should track active agreement classes", async() => {
    //
    //         await superToken.upgrade(INIT_BALANCE, {from: alice});
    //         await superToken.upgrade(INIT_BALANCE, {from: bob});
    //         let dataAgreement = cfa.contract.methods.createFlow(
    //             superToken.address,
    //             bob,
    //             FLOW_RATE.toString(),
    //             "0x"
    //         ).encodeABI();
    //         await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
    //             cfa.address,
    //             dataAgreement,
    //             {
    //                 from: alice,
    //             }
    //         );
    //
    //         const flowRate = await cfa.getNetFlow(superToken.address, bob);
    //         console.log("Bob flowRate: ", flowRate.toString());
    //         console.log("Check with ", FLOW_RATE.toString());
    //         assert.equal(flowRate.toString(), FLOW_RATE.toString(), "Not the same flow Rate");
    //
    //         dataAgreement = cfa.contract.methods.updateFlow(
    //             superToken.address,
    //             bob,
    //             FLOW_RATE.toString(),
    //             "0x"
    //         ).encodeABI();
    //         await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
    //             cfa.address,
    //             dataAgreement,
    //             {
    //                 from: alice,
    //             }
    //         );
    //
    //         let aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
    //         let bobAgreementClasses = await superToken.getAccountActiveAgreements.call(bob);
    //         let carolAgreementClasses = await superToken.getAccountActiveAgreements.call(carol);
    //
    //         assert.ok(aliceAgreementClasses.length == 1);
    //         assert.ok(bobAgreementClasses.length == 1);
    //         assert.ok(carolAgreementClasses.length == 0);
    //         assert.equal(aliceAgreementClasses[0], cfa.address);
    //         assert.equal(bobAgreementClasses[0], cfa.address);
    //
    //         dataAgreement = cfa.contract.methods.createFlow(
    //             superToken.address,
    //             carol,
    //             FLOW_RATE.mul(toBN(2)).toString(),
    //             "0x"
    //         ).encodeABI();
    //         await web3tx(superfluid.callAgreement, "Superfluid.callAgreement bob -> carol")(
    //             cfa.address,
    //             dataAgreement,
    //             {
    //                 from: bob,
    //             }
    //         );
    //
    //         aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
    //         bobAgreementClasses = await superToken.getAccountActiveAgreements.call(bob);
    //         carolAgreementClasses = await superToken.getAccountActiveAgreements.call(carol);
    //         assert.ok(aliceAgreementClasses.length == 1);
    //         assert.ok(bobAgreementClasses.length == 1);
    //         assert.ok(carolAgreementClasses.length == 1);
    //         assert.equal(aliceAgreementClasses[0], cfa.address);
    //         assert.equal(bobAgreementClasses[0], cfa.address);
    //         assert.equal(carolAgreementClasses[0], cfa.address);
    //
    //         dataAgreement = cfa.contract.methods.deleteFlow(
    //             superToken.address,
    //             alice,
    //             bob,
    //             "0x"
    //         ).encodeABI();
    //
    //         await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
    //             cfa.address,
    //             dataAgreement,
    //             {
    //                 from: alice,
    //             }
    //         );
    //
    //         aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
    //         bobAgreementClasses = await superToken.getAccountActiveAgreements.call(bob);
    //         aliceAgreementClasses = await superToken.getAccountActiveAgreements.call(alice);
    //         bobAgreementClasses = await superToken.getAccountActiveAgreements.call(bob);
    //         carolAgreementClasses = await superToken.getAccountActiveAgreements.call(carol);
    //         //FIXME THIS TEST
    //         //assert.ok(aliceAgreementClasses.length == 0);
    //         assert.ok(bobAgreementClasses.length == 1);
    //         assert.ok(carolAgreementClasses.length == 1);
    //         assert.equal(bobAgreementClasses[0], cfa.address);
    //         assert.equal(carolAgreementClasses[0], cfa.address);
    //
    //         await tester.validateSystem();
    //     });
    //
    //     //TODO Implement this check on solidity
    //     /*
    //     it("#2.2 - should only be updated by authorized agreement", async () => {
    //         await expectRevert(
    //             web3tx(superToken.updateAgreementAccountState,
    //                 "SuperToken.updateAgreementAccountState by alice directly")(
    //                 alice,
    //                 "0x42", {from: alice}
    //             ), "SuperToken: unauthorized agreement storage access");
    //     });
    //     */
    // });

    // describe("#6 - SuperToken.liquidateAgreement", () => {
    //     it("#6.1 - should make a liquidation", async() => {
    //         await web3tx(superToken.upgrade, "upgrade all from alice")(
    //             INIT_BALANCE, {from: alice});
    //         await web3tx(superToken.liquidateAgreement, "liquidate alice")(
    //             carol, "0x00000", alice, INIT_BALANCE);
    //         const balanceAlice = await superToken.balanceOf.call(alice);
    //         assert.equal(balanceAlice.toString(), "0", "alice balanceOf should be zero");
    //         await web3tx(superToken.liquidateAgreement, "liquidate alice")(
    //             carol, "0x00000", alice, toWad(1));
    //         const balanceCarol = await superToken.balanceOf.call(carol);
    //         assert.equal(balanceCarol.toString(),
    //             INIT_BALANCE.add(toWad(1)).toString(),
    //             "carol final balance not correct"
    //         );
    //     });
    // });

});
