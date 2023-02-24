// npx hardhat test test/contracts/apps/SuperAppBaseFlow.test.ts

import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
import {assert, ethers, expect} from "hardhat";

import {
    ConstantFlowAgreementV1,
    Superfluid,
    SuperTokenLibraryCFAMock,
    SuperTokenLibraryCFASuperAppMock,
    SuperTokenMock,
    SuperAppBaseFlowTester,
    InstantDistributionAgreementV1
} from "../../../typechain-types";
import TestEnvironment from "../../TestEnvironment";

const mintAmount = ethers.utils.parseEther("1000000000000000000000000000"); // a small loan of a billion dollars
const flowRate = "1000000000000";

const emptyBytes = "0x3078"; // ethers output for empty bytes 0x


describe("SuperAppBaseFlow testing", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();

    let cfa: ConstantFlowAgreementV1,
        ida: InstantDistributionAgreementV1,
        host: Superfluid,
        superAppBaseFlowTester: SuperAppBaseFlowTester,
        superToken: SuperTokenMock,
        superToken2: SuperTokenMock
    let alice: SignerWithAddress, bob: SignerWithAddress;

    before(async () => {

        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 3,
        });

        cfa = t.contracts.cfa;
        ida = t.contracts.ida;
        host = t.contracts.superfluid;

        let _alice
        let _bob;
        ({_alice, _bob} = t.aliases);
        alice = await ethers.getSigner(_alice);
        bob = await ethers.getSigner(_bob);   

        // Get Super Tokens
        const SuperTokenMockFactory = await ethers.getContractFactory(
            "SuperTokenMock"
        );
        superToken = await SuperTokenMockFactory.deploy(host.address, "69");
        await superToken.mintInternal(alice.address, mintAmount, "0x", "0x");
        await superToken.mintInternal(bob.address, mintAmount, "0x", "0x");

        // Get Super Tokens
        superToken2 = await SuperTokenMockFactory.deploy(host.address, "137");
        await superToken2.mintInternal(alice.address, mintAmount, "0x", "0x");
        await superToken2.mintInternal(bob.address, mintAmount, "0x", "0x");

        // Create an index for testing
        await host.callAgreement(
            ida.address,
            t.agreementHelper.idaInterface.encodeFunctionData(
                "createIndex",
                [
                    superToken.address,
                    "1",
                    "0x"
                ]
            ),
            "0x"
        );

    });

    beforeEach(async () => {


        // re-deploy the Super App Base Flow Tester
        const superAppBaseFlowTesterFactory = await ethers.getContractFactory(
            "SuperAppBaseFlowTester",
            alice
        );
    
        superAppBaseFlowTester = await superAppBaseFlowTesterFactory.deploy(
            host.address
        );

        // Clear Alice's flow rate
        let flowRate_ = (
            await cfa.getFlow(
                superToken.address,
                alice.address,
                superAppBaseFlowTester.address
            )
        ).flowRate.toString();

        if( flowRate_ != "0" ) {

            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "deleteFlow",
                    [
                        superToken.address,                 // super token
                        alice.address,                      // sender address
                        superAppBaseFlowTester.address,     // receiver address
                        "0x"                                // ctx
                    ]
                ),
                "0x"
            );
        
        }

    });

    describe("1 - Accepted Token Ops", async function () {
        it("1.1 - Set Accepted Token True", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                true
            );

            assert.equal(
                await superAppBaseFlowTester.isAcceptedSuperToken(superToken.address),
                true,
                "token acceptance not set"
            );

        });
        it("1.2 - Set Accepted Token False", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                false
            );

            assert.equal(
                await superAppBaseFlowTester.isAcceptedSuperToken(superToken.address),
                false,
                "token acceptance not set"
            );

        });
    });

    describe("2 - Unauthorized Host", async function () {
        it("2.1 - before create flow", async () => {

            await expect(
                superAppBaseFlowTester.connect(alice).beforeAgreementCreated(
                    superToken.address,
                    cfa.address,
                    ethers.constants.HashZero,
                    "0x",
                    "0x"
                )
            ).to.be.revertedWithCustomError(
                superAppBaseFlowTester,
                "UnauthorizedHost"
            );

        });
        it("2.2 - before update flow", async () => {

            await expect(
                superAppBaseFlowTester.connect(alice).beforeAgreementUpdated(
                    superToken.address,
                    cfa.address,
                    ethers.constants.HashZero,
                    "0x",
                    "0x"
                )
            ).to.be.revertedWithCustomError(
                superAppBaseFlowTester,
                "UnauthorizedHost"
            );

        });
        it("2.3 - before delete flow", async () => {

            await expect(
                await superAppBaseFlowTester.connect(alice).beforeAgreementTerminated(
                    superToken.address,
                    cfa.address,
                    ethers.constants.HashZero,
                    "0x",
                    "0x"
                )
            ).to.eq(
                emptyBytes
            );

        });
        it("2.4 - after create flow", async () => {

            await expect(
                superAppBaseFlowTester.connect(alice).afterAgreementCreated(
                    superToken.address,
                    cfa.address,
                    ethers.constants.HashZero,
                    "0x",
                    "0x",
                    "0x"
                )
            ).to.be.revertedWithCustomError(
                superAppBaseFlowTester,
                "UnauthorizedHost"
            );

        });
        it("2.5 - after update flow", async () => {

            await expect(
                superAppBaseFlowTester.connect(alice).afterAgreementUpdated(
                    superToken.address,
                    cfa.address,
                    ethers.constants.HashZero,
                    "0x",
                    "0x",
                    "0x"
                )
            ).to.be.revertedWithCustomError(
                superAppBaseFlowTester,
                "UnauthorizedHost"
            );

        });
        it("2.6 - after delete flow", async () => {
            
            await superAppBaseFlowTester.connect(alice).afterAgreementTerminated(
                superToken.address,
                cfa.address,
                ethers.constants.HashZero,
                "0x47DE",
                "0x47DE",
                "0x47DE"
            );   

            // afterContextHolder shouldn't have reached modification
            await expect(
                await superAppBaseFlowTester.connect(alice).afterCtxHolder()
            ).to.eq(
                "0x"
            );

        });
    });

    describe("3 - Unaccepted Token", async function () {
        it("3.1 - create flow", async () => {

            // create flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "createFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        flowRate,
                        "0x"
                    ]
                ),
                "0x"
            );

            // context shouldn't reach modification from beforeFlowCreated 
            await expect(
                await superAppBaseFlowTester.afterCtxHolder()
            ).to.eq(
                "0x"
            );

        });
    });

    describe("4 - Unaccepted Agreement", async function () {
        it("4.1 - create flow", async () => {

            // issue shares (a "agreement creation")
            await host
            .connect(alice)
            .callAgreement(
                ida.address,
                t.agreementHelper.idaInterface.encodeFunctionData(
                    "updateSubscription",
                    [
                        superToken.address,
                        "1",
                        superAppBaseFlowTester.address,
                        "100",
                        "0x" // ctx placeholder
                    ]
                ),
                "0x"
            );

            // context shouldn't reach modification from beforeFlowCreated 
            await expect(
                await superAppBaseFlowTester.afterCtxHolder()
            ).to.eq(
                "0x"
            );
            // creation callback should have been triggered
            await expect(
                await superAppBaseFlowTester.callbackHolder()
            ).to.eq(
                "create"
            );

        });
        it("4.2 - update flow", async () => {

            // issue shares (a "agreement creation")
            await host
            .connect(alice)
            .callAgreement(
                ida.address,
                t.agreementHelper.idaInterface.encodeFunctionData(
                    "updateSubscription",
                    [
                        superToken.address,
                        "1",
                        superAppBaseFlowTester.address,
                        "100",
                        "0x" // ctx placeholder
                    ]
                ),
                "0x"
            );

            await superAppBaseFlowTester.connect(alice).clearHolders();

            // issue shares (a "agreement update"... hopefully)
            await host
            .connect(alice)
            .callAgreement(
                ida.address,
                t.agreementHelper.idaInterface.encodeFunctionData(
                    "updateSubscription",
                    [
                        superToken.address,
                        "1",
                        superAppBaseFlowTester.address,
                        "107",
                        "0x" // ctx placeholder
                    ]
                ),
                "0x"
            );

            // context shouldn't reach modification from beforeFlowCreated 
            await expect(
                await superAppBaseFlowTester.afterCtxHolder()
            ).to.eq(
                "0x"
            );
            // update callback should have been triggered
            await expect(
                await superAppBaseFlowTester.callbackHolder()
            ).to.eq(
                "update"
            );

        });
        it("4.3 - delete flow", async () => {

            // update subscription
            await host
            .connect(alice)
            .callAgreement(
                ida.address,
                t.agreementHelper.idaInterface.encodeFunctionData(
                    "updateSubscription",
                    [
                        superToken.address,
                        "1",
                        superAppBaseFlowTester.address,
                        "100",
                        "0x" // ctx placeholder
                    ]
                ),
                "0x"
            );

            await superAppBaseFlowTester.connect(alice).clearHolders();

            // delete subscription
            await host
            .connect(alice)
            .callAgreement(
                ida.address,
                t.agreementHelper.idaInterface.encodeFunctionData(
                    "deleteSubscription",
                    [
                        superToken.address,
                        alice.address,
                        "1",
                        superAppBaseFlowTester.address,
                        "0x" // ctx placeholder
                    ]
                ),
                "0x"
            );

            // context shouldn't reach modification from beforeFlowDeleted 
            await expect(
                await superAppBaseFlowTester.afterCtxHolder()
            ).to.eq(
                "0x"
            );
            // creation callback should have been triggered
            await expect(
                await superAppBaseFlowTester.callbackHolder()
            ).to.eq(
                "delete"
            );

        });
    });

    describe("5 - Correct Callback Data", async function () {
        it("5.1 - create flow", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                true
            );

            // create flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "createFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        flowRate,
                        "0x"
                    ]
                ),
                "0x"
            );

            // sender in callback should be alice
            await expect(
                await superAppBaseFlowTester.afterSenderHolder()
            ).to.eq(
                alice.address
            );
            // old flowrate should be zero
            await expect(
                await superAppBaseFlowTester.oldFlowRateHolder()
            ).to.eq(
                "0"
            );
            // old update timestamp to be zero
            await expect(
                await superAppBaseFlowTester.lastUpdateHolder()
            ).to.eq(
                "0"
            );

        });
        it("5.2 - update flow", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                true
            );

            // create flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "createFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        flowRate,
                        "0x"
                    ]
                ),
                "0x"
            );

            const lastUpdate = (await ethers.provider.getBlock(await ethers.provider.getBlockNumber())).timestamp;

            // update flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "updateFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        (parseInt(flowRate)*2).toString(), // doubled flow rate
                        "0x"
                    ]
                ),
                "0x"
            );

            // sender in callback should be alice
            await expect(
                await superAppBaseFlowTester.afterSenderHolder()
            ).to.eq(
                alice.address
            );
            // old flowrate should be flowRate
            await expect(
                await superAppBaseFlowTester.oldFlowRateHolder()
            ).to.eq(
                flowRate
            );
            // old update timestamp to be last update
            await expect(
                await superAppBaseFlowTester.lastUpdateHolder()
            ).to.eq(
                lastUpdate
            );

        });
        it("5.3 - delete flow", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                true
            );

            // create flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "createFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        flowRate,
                        "0x"
                    ]
                ),
                "0x"
            );

            const lastUpdate = (await ethers.provider.getBlock(await ethers.provider.getBlockNumber())).timestamp;

            // delete flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "deleteFlow",
                    [
                        superToken.address,
                        alice.address,
                        superAppBaseFlowTester.address,
                        "0x"
                    ]
                ),
                "0x"
            );


            // receiver in callback should be super app
            await expect(
                await superAppBaseFlowTester.afterReceiverHolder()
            ).to.eq(
                superAppBaseFlowTester.address
            );
            // sender in callback should be alice
            await expect(
                await superAppBaseFlowTester.afterSenderHolder()
            ).to.eq(
                alice.address
            );
            // old flowrate should be flowRate
            await expect(
                await superAppBaseFlowTester.oldFlowRateHolder()
            ).to.eq(
                flowRate
            );
            // old update timestamp to be last update
            await expect(
                await superAppBaseFlowTester.lastUpdateHolder()
            ).to.eq(
                lastUpdate
            );

        });
        it("5.4 - rogue delete", async () => {

            await superAppBaseFlowTester.connect(alice).setAcceptedSuperToken(
                superToken.address,
                true
            );

            // transfer in tokens so contract has enough to stream
            await superToken.transfer(superAppBaseFlowTester.address, ethers.utils.parseEther("100"));

            // start stream from contract
            await superAppBaseFlowTester.connect(alice).startStream(
                superToken.address,
                alice.address,
                flowRate
            );

            const lastUpdate = (await ethers.provider.getBlock(await ethers.provider.getBlockNumber())).timestamp;

            // delete flow
            await host
            .connect(alice)
            .callAgreement(
                cfa.address,
                t.agreementHelper.cfaInterface.encodeFunctionData(
                    "deleteFlow",
                    [
                        superToken.address,
                        superAppBaseFlowTester.address,
                        alice.address,
                        "0x"
                    ]
                ),
                "0x"
            );

            // sender in callback should be super app
            await expect(
                await superAppBaseFlowTester.afterSenderHolder()
            ).to.eq(
                superAppBaseFlowTester.address
            );
            // receiver in callback should be alice
            await expect(
                await superAppBaseFlowTester.afterReceiverHolder()
            ).to.eq(
                alice.address
            );
            // sender in callback should be **superapp**
            await expect(
                await superAppBaseFlowTester.afterSenderHolder()
            ).to.eq(
                superAppBaseFlowTester.address
            );
            // old flowrate should be flowRate
            await expect(
                await superAppBaseFlowTester.oldFlowRateHolder()
            ).to.eq(
                flowRate
            );
            // old update timestamp to be last update
            await expect(
                await superAppBaseFlowTester.lastUpdateHolder()
            ).to.eq(
                lastUpdate
            );
            
        })
    });

});