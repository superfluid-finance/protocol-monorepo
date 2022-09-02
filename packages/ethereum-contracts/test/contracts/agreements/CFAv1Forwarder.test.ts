const TestEnvironment = require("../../TestEnvironment");
const {expectCustomError} = require("../../utils/expectRevert");
import {assert, ethers} from "hardhat";
import {expect} from "chai";
import {
    CFAv1Forwarder,
    ConstantFlowAgreementV1,
    Superfluid,
    SuperTokenMock,
    TestGovernance,
} from "../../../typechain-types";
import {SignerWithAddress} from "@nomiclabs/hardhat-ethers/signers";
const {toBN} = require("../utils/helpers");

const mintAmount = "1000000000000000000000000000"; // a small loan of a billion dollars
const flowrate = "1000000000000";
const flowrate2 = "2000000000000";

describe("Agreement Forwarder", function () {
    const t = TestEnvironment.getSingleton();
    const {ZERO_ADDRESS} = t.constants;
    let SuperTokenMock: SuperTokenMock,
        host: Superfluid,
        cfa: ConstantFlowAgreementV1,
        governance: TestGovernance,
        cfaFwd: CFAv1Forwarder;
    let alice: string, bob: string, carol: string;
    let aliceSigner: SignerWithAddress,
        bobSigner: SignerWithAddress,
        carolSigner: SignerWithAddress;

    // ACL flags
    const ALLOW_CREATE = 1 << 0;
    const ALLOW_UPDATE = 1 << 1;
    const ALLOW_DELETE = 1 << 2;

    before(async () => {
        await t.beforeTestSuite({
            isTruffle: true,
            nAccounts: 4,
        });

        cfa = t.contracts.cfa;
        host = t.contracts.superfluid;
        governance = t.contracts.governance;

        const CFAv1ForwarderFactory = await ethers.getContractFactory(
            "CFAv1Forwarder"
        );
        cfaFwd = await CFAv1ForwarderFactory.deploy(host.address);

        await governance.enableTrustedForwarder(
            host.address,
            ZERO_ADDRESS,
            cfaFwd.address
        );

        ({alice, bob, carol} = t.aliases);
        aliceSigner = await ethers.getSigner(alice);
        bobSigner = await ethers.getSigner(bob);
        carolSigner = await ethers.getSigner(carol);
    });

    beforeEach(async () => {
        const SuperTokenMockFactory = await ethers.getContractFactory(
            "SuperTokenMock"
        );
        SuperTokenMock = await SuperTokenMockFactory.deploy(host.address, "69");
        await SuperTokenMock.mintInternal(alice, mintAmount, "0x", "0x");
        await SuperTokenMock.mintInternal(bob, mintAmount, "0x", "0x");
    });

    describe("Convenience methods", async function () {
        it("Revert on negative flowrate", async () => {
            await expectCustomError(
                cfaFwd
                    .connect(aliceSigner)
                    .setFlowrate(SuperTokenMock.address, bob, -1),
                cfaFwd,
                "CFA_FWD_INVALID_FLOW_RATE"
            );
        });

        it("On failure: provide correct error", async () => {
            await expectCustomError(
                cfaFwd
                    .connect(aliceSigner)
                    .setFlowrate(SuperTokenMock.address, alice, flowrate),
                cfa,
                "CFA_NO_SELF_FLOW"
            );
        });

        it("Set flow without pre-existing flow (create)", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );
        });

        it("Set flow with pre-existing flow (update)", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate);

            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate2);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );
        });

        it("Set flow to 0 with pre-existing flow (delete)", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate);

            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, 0);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("Set flow to 0 without pre-existing flow (do nothing)", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, 0);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("Set flow with same flowrate as before (do nothing)", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate);
            await cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );
        });

        it("Grant all flowOperator permissions", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .grantPermissions(SuperTokenMock.address, carol);

            // bob is not authorized
            await expectCustomError(
                cfaFwd
                    .connect(bobSigner)
                    .setFlowrateFrom(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate
                    ),
                cfa,
                "CFA_ACL_OPERATOR_NO_CREATE_PERMISSIONS"
            );

            // create
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(SuperTokenMock.address, alice, bob, flowrate);
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            // update
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(SuperTokenMock.address, alice, bob, flowrate2);
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            // delete
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(SuperTokenMock.address, alice, bob, 0);
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("Revoke all flowOperator permissions", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .grantPermissions(SuperTokenMock.address, carol);

            // create
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(SuperTokenMock.address, alice, bob, flowrate);

            await cfaFwd
                .connect(aliceSigner)
                .revokePermissions(SuperTokenMock.address, carol);

            // update denied
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate2
                    ),
                cfa,
                "CFA_ACL_OPERATOR_NO_UPDATE_PERMISSIONS"
            );

            // delete denied
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(SuperTokenMock.address, alice, bob, 0),
                cfa,
                "CFA_ACL_OPERATOR_NO_DELETE_PERMISSIONS"
            );
        });
    });

    describe("Wrapper methods", async function () {
        it("updateFlowOperatorPermissions: enforce flowrateAllowance", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .updateFlowOperatorPermissions(
                    SuperTokenMock.address,
                    carol,
                    ALLOW_CREATE | ALLOW_UPDATE,
                    flowrate
                );

            // create with excess flowrate
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate2
                    ),
                cfa,
                "CFA_ACL_FLOW_RATE_ALLOWANCE_EXCEEDED"
            );

            // create with allowed flowrate
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    Number(flowrate) - 1
                );

            // update with excess flowrate
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate2
                    ),
                cfa,
                "CFA_ACL_FLOW_RATE_ALLOWANCE_EXCEEDED"
            );

            // update with allowed flowrate
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    Number(flowrate) - 1
                );

            // permission for close not granted
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(SuperTokenMock.address, alice, bob, 0),
                cfa,
                "CFA_ACL_OPERATOR_NO_DELETE_PERMISSIONS"
            );
        });

        it("updateFlowOperatorPermissions: can only delete", async () => {
            // alice starts flow to bob...
            cfaFwd
                .connect(aliceSigner)
                .setFlowrate(SuperTokenMock.address, bob, flowrate2),
                // and gives carol permission to delete flows
                await cfaFwd
                    .connect(aliceSigner)
                    .updateFlowOperatorPermissions(
                        SuperTokenMock.address,
                        carol,
                        ALLOW_DELETE,
                        flowrate
                    );

            // carol: no update permission
            await expectCustomError(
                cfaFwd
                    .connect(carolSigner)
                    .setFlowrateFrom(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        Number(flowrate) - 1
                    ),
                cfa,
                "CFA_ACL_OPERATOR_NO_UPDATE_PERMISSIONS"
            );

            // but can delete
            await cfaFwd
                .connect(carolSigner)
                .setFlowrateFrom(SuperTokenMock.address, alice, bob, 0);

            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("create/update/delete Flow", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            await cfaFwd
                .connect(aliceSigner)
                .updateFlow(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    flowrate2,
                    "0x"
                );
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            await cfaFwd
                .connect(aliceSigner)
                .deleteFlow(SuperTokenMock.address, alice, bob, "0x");
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("create/update/delete Flow by flowOperator", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .grantPermissions(SuperTokenMock.address, carol);

            await cfaFwd
                .connect(carolSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate
            );

            await cfaFwd
                .connect(carolSigner)
                .updateFlow(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    flowrate2,
                    "0x"
                );
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                flowrate2
            );

            await cfaFwd
                .connect(carolSigner)
                .deleteFlow(SuperTokenMock.address, alice, bob, "0x");
            assert.equal(
                (
                    await cfa.getFlow(SuperTokenMock.address, alice, bob)
                ).flowRate.toString(),
                "0"
            );
        });

        it("create/update/delete Flow with userData", async () => {
            await expect(
                cfaFwd
                    .connect(aliceSigner)
                    .createFlow(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate,
                        "0xd1"
                    )
            )
                .to.emit(cfa, "FlowUpdated")
                .withArgs(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    flowrate,
                    toBN(flowrate).mul(toBN(-1)),
                    flowrate,
                    "0xd1"
                );

            await expect(
                cfaFwd
                    .connect(aliceSigner)
                    .updateFlow(
                        SuperTokenMock.address,
                        alice,
                        bob,
                        flowrate2,
                        "0xd2"
                    )
            )
                .to.emit(cfa, "FlowUpdated")
                .withArgs(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    flowrate2,
                    toBN(flowrate2).mul(toBN(-1)),
                    flowrate2,
                    "0xd2"
                );

            await expect(
                cfaFwd
                    .connect(aliceSigner)
                    .deleteFlow(SuperTokenMock.address, alice, bob, "0xd3")
            )
                .to.emit(cfa, "FlowUpdated")
                .withArgs(
                    SuperTokenMock.address,
                    alice,
                    bob,
                    "0",
                    "0",
                    "0",
                    "0xd3"
                );
        });
    });

    describe("Readonly methods", async function () {
        it("getFlowrate", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");
            assert.equal(
                (
                    await cfaFwd.getFlowrate(SuperTokenMock.address, alice, bob)
                ).toString(),
                flowrate
            );
        });

        it("getFlowInfo", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");

            const flowInfo = await cfaFwd.getFlowInfo(
                SuperTokenMock.address,
                alice,
                bob
            );
            const flow = await cfa.getFlow(SuperTokenMock.address, alice, bob);

            assert.equal(
                flowInfo.lastUpdated.toString(),
                flow.timestamp.toString()
            );
            assert.equal(
                flowInfo.flowrate.toString(),
                flow.flowRate.toString()
            );
            assert.equal(flowInfo.deposit.toString(), flow.deposit.toString());
            assert.equal(
                flowInfo.owedDeposit.toString(),
                flow.owedDeposit.toString()
            );
        });

        it("getBufferAmountByFlowrate", async () => {
            const ba = await cfa.getDepositRequiredForFlowRate(
                SuperTokenMock.address,
                flowrate
            );
            const baW = await cfaFwd.getBufferAmountByFlowrate(
                SuperTokenMock.address,
                flowrate
            );
            assert.equal(ba.toString(), baW.toString());
        });

        it("getAccountFlowrate", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");
            await cfaFwd
                .connect(bobSigner)
                .createFlow(
                    SuperTokenMock.address,
                    bob,
                    alice,
                    Number(flowrate) - 1,
                    "0x"
                );

            const accFrAlice = await cfaFwd.getAccountFlowrate(
                SuperTokenMock.address,
                alice
            );
            const accFrBob = await cfaFwd.getAccountFlowrate(
                SuperTokenMock.address,
                bob
            );

            assert.equal(accFrAlice.toString(), "-1");
            assert.equal(accFrBob.toString(), "1");
        });

        it("getAccountFlowInfo", async () => {
            await cfaFwd
                .connect(aliceSigner)
                .createFlow(SuperTokenMock.address, alice, bob, flowrate, "0x");
            await cfaFwd
                .connect(bobSigner)
                .createFlow(
                    SuperTokenMock.address,
                    bob,
                    alice,
                    Number(flowrate) - 1,
                    "0x"
                );

            const accFIAliceI = await cfa.getAccountFlowInfo(
                SuperTokenMock.address,
                alice
            );
            const accFIAliceW = await cfaFwd.getAccountFlowInfo(
                SuperTokenMock.address,
                alice
            );
            const accFIBobI = await cfa.getAccountFlowInfo(
                SuperTokenMock.address,
                bob
            );
            const accFIBobW = await cfaFwd.getAccountFlowInfo(
                SuperTokenMock.address,
                bob
            );

            assert.equal(
                accFIAliceW.lastUpdated.toString(),
                accFIAliceI.timestamp.toString()
            );
            assert.equal(
                accFIAliceW.flowrate.toString(),
                accFIAliceI.flowRate.toString()
            );
            assert.equal(
                accFIAliceW.deposit.toString(),
                accFIAliceI.deposit.toString()
            );
            assert.equal(
                accFIAliceW.owedDeposit.toString(),
                accFIAliceI.owedDeposit.toString()
            );

            assert.equal(
                accFIBobW.lastUpdated.toString(),
                accFIBobI.timestamp.toString()
            );
            assert.equal(
                accFIBobW.flowrate.toString(),
                accFIBobI.flowRate.toString()
            );
            assert.equal(
                accFIBobW.deposit.toString(),
                accFIBobI.deposit.toString()
            );
            assert.equal(
                accFIBobW.owedDeposit.toString(),
                accFIBobI.owedDeposit.toString()
            );
        });

        it("getFlowOperatorPermissions", async () => {
            const permPre = await cfaFwd.getFlowOperatorPermissions(
                SuperTokenMock.address,
                alice,
                carol
            );
            assert.equal(permPre.permissions, 0);
            assert.equal(permPre.flowrateAllowance.toString(), "0");

            await cfaFwd
                .connect(aliceSigner)
                .updateFlowOperatorPermissions(
                    SuperTokenMock.address,
                    carol,
                    ALLOW_DELETE,
                    flowrate
                );

            const permPost = await cfaFwd.getFlowOperatorPermissions(
                SuperTokenMock.address,
                alice,
                carol
            );
            assert.equal(permPost.permissions, ALLOW_DELETE);
            assert.equal(permPost.flowrateAllowance.toString(), flowrate);
        });
    });
});
