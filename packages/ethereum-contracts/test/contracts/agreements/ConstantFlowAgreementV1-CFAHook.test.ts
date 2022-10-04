import {ethers, expect} from "hardhat";

import {
    BadCFAHookMock,
    ConstantFlowAgreementV1,
    ConstantFlowAgreementV1__factory,
    GoodCFAHookMock,
    SuperTokenMock,
} from "../../../typechain-types";
import {ConstantFlowAgreementV1 as HookMockCFAv1} from "../../../typechain-types/contracts/mocks/CFAHookMocks.sol/BaseCFAHookMock";
import TestEnvironment from "../../TestEnvironment";
import {expectCustomError} from "../../utils/expectRevert";
import {toBN} from "../utils/helpers";

import AgreementHelper, {
    FLOW_TYPE_CREATE,
    FLOW_TYPE_DELETE,
    FLOW_TYPE_UPDATE,
} from "./AgreementHelper";
import {expectNetFlow} from "./ConstantFlowAgreementV1.behavior";

const EMPTY_MOCK_FLOW_PARAMS: HookMockCFAv1.FlowParamsStruct = {
    flowId: ethers.utils.formatBytes32String("gm"), // flowId is 32 bytes, invalid bytes length otherwise
    flowOperator: ethers.constants.AddressZero,
    flowRate: toBN(0),
    sender: ethers.constants.AddressZero,
    receiver: ethers.constants.AddressZero,
    userData: "0x",
};

describe("CFAv1 | CFA Hook Mock Tests", function () {
    this.timeout(300e3);
    const t = TestEnvironment.getSingleton();
    let agreementHelper: AgreementHelper;
    const {FLOW_RATE1} = t.configs;

    let alice: string, bob: string;
    let superToken: SuperTokenMock;
    let CFAFactory: ConstantFlowAgreementV1__factory;
    let CFAWithLogicHook: ConstantFlowAgreementV1;
    let GoodCFAHookMock: GoodCFAHookMock;
    let BadCFAHookMock: BadCFAHookMock;

    describe("#1 GoodCFAHookMock Tests", () => {
        before(async () => {
            process.env.USE_GOOD_HOOK = "true";
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 5,
            });
            ({alice, bob} = t.aliases);

            superToken = t.tokens.SuperToken;
            agreementHelper = t.agreementHelper;
            CFAFactory = await ethers.getContractFactory(
                "ConstantFlowAgreementV1"
            );
        });

        beforeEach(async () => {
            await t.beforeEachTestCase();
            const GoodCFAHookMockFactory = await ethers.getContractFactory(
                "GoodCFAHookMock"
            );
            GoodCFAHookMock = await GoodCFAHookMockFactory.deploy();
            CFAWithLogicHook = await CFAFactory.deploy(
                t.contracts.superfluid.address,
                GoodCFAHookMock.address
            );
            await t.contracts.governance.updateContracts(
                t.contracts.superfluid.address,
                ethers.constants.AddressZero,
                [CFAWithLogicHook.address],
                ethers.constants.AddressZero
            );
        });

        // The final Hook contract should test this and adhere to the onlyCFA and onlyOwner rule.
        describe("#1.1 Only CFA", () => {
            it("#1.1.1 Should revert if non-CFA address tries to call the create hook", async () => {
                await expectCustomError(
                    GoodCFAHookMock.onCreate(
                        superToken.address,
                        EMPTY_MOCK_FLOW_PARAMS
                    ),
                    GoodCFAHookMock,
                    "NOT_CFA"
                );
            });

            it("#1.1.2 Should revert if non-CFA address tries to call the update hook", async () => {
                await expectCustomError(
                    GoodCFAHookMock.onUpdate(
                        superToken.address,
                        EMPTY_MOCK_FLOW_PARAMS,
                        toBN(0)
                    ),
                    GoodCFAHookMock,
                    "NOT_CFA"
                );
            });

            it("#1.1.3 Should revert if non-CFA address tries to call the delete hook", async () => {
                await expectCustomError(
                    GoodCFAHookMock.onDelete(
                        superToken.address,
                        EMPTY_MOCK_FLOW_PARAMS,
                        toBN(0)
                    ),
                    GoodCFAHookMock,
                    "NOT_CFA"
                );
            });

            it("#1.1.4 Should revert if non owner attempts to set CFA on hook mock", async () => {
                await expectCustomError(
                    GoodCFAHookMock.connect(await ethers.getSigner(bob)).setCFA(
                        bob
                    ),
                    GoodCFAHookMock,
                    "NOT_OWNER"
                );
            });
        });

        describe("#1.2 Hook should execute as expected", () => {
            beforeEach(async () => {
                // set the CFA so that onlyCFA will be allowed
                await GoodCFAHookMock.setCFA(t.contracts.cfa.address);

                await t.upgradeBalance("alice", t.configs.INIT_BALANCE);
            });

            it("#1.2.1 Should execute create hook when flow is created", async () => {
                await expect(
                    agreementHelper.modifyFlow({
                        type: FLOW_TYPE_CREATE,
                        superToken: superToken.address,
                        sender: alice,
                        receiver: bob,
                        flowRate: FLOW_RATE1,
                    })
                )
                    .to.emit(GoodCFAHookMock, "OnCreateEvent")
                    .withArgs(
                        superToken.address,
                        alice,
                        bob,
                        alice,
                        FLOW_RATE1
                    );
            });
            it("#1.2.2 Should execute update hook when flow is updated", async () => {
                await agreementHelper.modifyFlow({
                    type: FLOW_TYPE_CREATE,
                    superToken: superToken.address,
                    sender: alice,
                    receiver: bob,
                    flowRate: FLOW_RATE1,
                });

                await expect(
                    agreementHelper.modifyFlow({
                        type: FLOW_TYPE_UPDATE,
                        superToken: superToken.address,
                        sender: alice,
                        receiver: bob,
                        flowRate: FLOW_RATE1.mul(toBN(2)),
                    })
                )
                    .to.emit(GoodCFAHookMock, "OnUpdateEvent")
                    .withArgs(
                        superToken.address,
                        alice,
                        bob,
                        alice,
                        FLOW_RATE1.mul(toBN(2)),
                        FLOW_RATE1
                    );
            });

            it("#1.2.3 Should execute delete hook when flow is deleted", async () => {
                await agreementHelper.modifyFlow({
                    type: FLOW_TYPE_CREATE,
                    superToken: superToken.address,
                    sender: alice,
                    receiver: bob,
                    flowRate: FLOW_RATE1,
                });

                await expect(
                    agreementHelper.modifyFlow({
                        type: FLOW_TYPE_DELETE,
                        superToken: superToken.address,
                        sender: alice,
                        receiver: bob,
                    })
                )
                    .to.emit(GoodCFAHookMock, "OnDeleteEvent")
                    .withArgs(
                        superToken.address,
                        alice,
                        bob,
                        alice,
                        "0",
                        FLOW_RATE1
                    );
            });
        });
    });

    describe("#2 BadCFAHookMock Tests", () => {
        before(async () => {
            process.env.USE_GOOD_HOOK = "";
            await t.beforeTestSuite({
                isTruffle: true,
                nAccounts: 5,
            });
            ({alice, bob} = t.aliases);

            superToken = t.tokens.SuperToken;
            agreementHelper = t.agreementHelper;
        });

        beforeEach(async () => {
            await t.beforeEachTestCase();
            await t.upgradeBalance("alice", t.configs.INIT_BALANCE);

            const BadCFAHookMockFactory = await ethers.getContractFactory(
                "BadCFAHookMock"
            );
            BadCFAHookMock = await BadCFAHookMockFactory.deploy();
            CFAWithLogicHook = await CFAFactory.deploy(
                t.contracts.superfluid.address,
                BadCFAHookMock.address
            );
            await t.contracts.governance.updateContracts(
                t.contracts.superfluid.address,
                ethers.constants.AddressZero,
                [CFAWithLogicHook.address],
                ethers.constants.AddressZero
            );

            // Create flow before each and we should be using BadCFAHookMock here
            await agreementHelper.modifyFlow({
                type: FLOW_TYPE_CREATE,
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: FLOW_RATE1,
            });
        });

        it("#2.1 Should still create flow despite revert in hook contract", async () => {
            await expectNetFlow({
                testenv: t,
                account: "alice",
                superToken,
                value: FLOW_RATE1.mul(toBN(-1)),
            });
            await expectNetFlow({
                testenv: t,
                account: "bob",
                superToken,
                value: FLOW_RATE1,
            });
        });

        it("#2.2 Should still update flow despite revert in hook contract", async () => {
            await agreementHelper.modifyFlow({
                type: FLOW_TYPE_UPDATE,
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
                flowRate: FLOW_RATE1.mul(toBN(2)),
            });

            await expectNetFlow({
                testenv: t,
                account: "alice",
                superToken,
                value: FLOW_RATE1.mul(toBN(-2)),
            });
            await expectNetFlow({
                testenv: t,
                account: "bob",
                superToken,
                value: FLOW_RATE1.mul(toBN(2)),
            });
        });

        it("#2.3 Should still delete flow despite revert in hook contract", async () => {
            await agreementHelper.modifyFlow({
                type: FLOW_TYPE_DELETE,
                superToken: superToken.address,
                sender: alice,
                receiver: bob,
            });

            await expectNetFlow({
                testenv: t,
                account: "alice",
                superToken,
                value: toBN(0),
            });
            await expectNetFlow({
                testenv: t,
                account: "bob",
                superToken,
                value: toBN(0),
            });
        });
    });
});
