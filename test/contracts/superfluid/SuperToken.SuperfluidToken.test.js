// const {
//     expectRevert
//     // expectEvent
// } = require("@openzeppelin/test-helpers");

const {
    web3tx,
    toBN
    // toDecimals,
    // toBN
} = require("@decentral.ee/web3-helpers");

const traveler = require("ganache-time-traveler");

const TestEnvironment = require("../../TestEnvironment");

const ADV_TIME = 2;
const FLOW_RATE = toBN("10000000000000");

contract("SuperToken's SuperfluidToken implementation", accounts => {

    const t = new TestEnvironment(accounts.slice(0, 4));
    const { alice, bob, carol} = t.aliases;
    const { INIT_BALANCE } = t.constants;

    // let token;
    let superToken;
    let cfa;
    let superfluid;

    before(async () => {
        await t.reset();
        ({
            superfluid,
            cfa,
        } = t.contracts);
    });

    beforeEach(async function () {
        await t.createNewToken({ doUpgrade: false });
        ({
            superToken,
        } = t.contracts);
    });

    describe("#0 SuperToken misc", () => {
        it("#0.1 - test basic token info", async () => {
            assert.equal(await superToken.name.call(), "Super Test Token");
            assert.equal(await superToken.symbol.call(), "TESTx");
            assert.equal(await superToken.decimals.call(), 18);
        });

        it("#0.2 validate immutable storage layout", async () => {
            const SuperTokenMock = artifacts.require("SuperTokenMock");
            const tester = await SuperTokenMock.new();
            await tester.validateStorageLayout.call();
        });
    });

    // describe("#3 SuperToken ISuperAgreementStorage(TBD) operations", () => {
    //     // TODO To be improved with a mock agreement class
    //
    //     it("#3.1 - should track active agreement classes", async() => {
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
    //     it("#3.2 - should only be updated by authorized agreement", async () => {
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


    describe("#1 SuperToken.transfer", () => {

        it("#1.3 - should be able to transfer flow balance", async() => {
            await web3tx(superToken.upgrade, "upgrade all from alice")(
                INIT_BALANCE, {from: alice});

            const dataAgreement = cfa.contract.methods.createFlow(
                superToken.address,
                bob,
                FLOW_RATE.toString(),
                "0x"
            ).encodeABI();

            await web3tx(superfluid.callAgreement, "Superfluid.callAgreement alice -> bob")(
                cfa.address,
                dataAgreement,
                {
                    from: alice,
                }
            );

            await traveler.advanceTimeAndBlock(ADV_TIME);

            const superBalanceBob = await superToken.balanceOf.call(bob);
            await web3tx(superToken.transfer, "downgrade all interim balance from bob to carol")(
                carol, superBalanceBob, {from: bob});

            const superBalanceCarol = await superToken.balanceOf.call(carol);
            assert.equal(superBalanceCarol.toString(), superBalanceBob.toString());

            await t.validateSystem();
        });
    });
});
