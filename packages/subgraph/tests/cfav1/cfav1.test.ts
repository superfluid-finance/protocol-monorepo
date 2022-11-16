import { Address, BigInt } from "@graphprotocol/graph-ts";
import {
    assert,
    beforeEach,
    clearStore,
    describe,
    test,
} from "matchstick-as/assembly/index";
import { FlowUpdated } from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import {
    handleFlowOperatorUpdated,
    handleFlowUpdated,
} from "../../src/mappings/cfav1";
import { BIG_INT_ZERO, getAccountTokenSnapshotID, getStreamID, ZERO_ADDRESS } from "../../src/utils";
import {
    assertEventBaseProperties,
    assertHigherOrderBaseProperties,
} from "../assertionHelpers";
import { alice, bob, DEFAULT_DECIMALS, LIQUIDATION_PERIOD, maticXAddress, maticXName, maticXSymbol } from "../constants";
import { stringToBytes } from "../converters";
import { mockedHandleFlowUpdatedRPCCalls } from "../mockedFunctions";
import {
    createFlowOperatorUpdatedEvent,
    createFlowUpdatedEvent,
    getFlowOperatorId,
} from "./cfav1.helper";

const initialFlowRate = BigInt.fromI32(100);

describe("ConstantFlowAgreementV1 Mapper Unit Tests", () => {
    describe("Event Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleFlowUpdated() - Should create a new FlowUpdatedEvent entity (create)", () => {
            // create flow
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "0",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                initialFlowRate,    // flowRate
                BIG_INT_ZERO,       // previousSenderFlowRate
                BIG_INT_ZERO,       // previousReceiverFlowRate
                true                // isListed
            );
        });

        test("handleFlowUpdated() - Should create new FlowUpdatedEvent entities (create => update)", () => {
            // create flow
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "0",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                initialFlowRate,    // flowRate
                BIG_INT_ZERO,       // previousSenderFlowRate
                BIG_INT_ZERO,       // previousReceiverFlowRate
                true                // isListed
            );

            // update flow: increase flow rate
            const increasedFlowRate = initialFlowRate.plus(BigInt.fromI32(50));
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "1",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                increasedFlowRate,  // flowRate
                initialFlowRate,    // previousSenderFlowRate
                initialFlowRate,    // previousReceiverFlowRate
                true                // isListed
            );

            // update flow: decrease flow rate
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "1",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                initialFlowRate,    // flowRate
                increasedFlowRate,  // previousSenderFlowRate
                increasedFlowRate,  // previousReceiverFlowRate
                true // isListed
            );
        });

        test("handleFlowUpdated() - Should create new FlowUpdatedEvent entities (create => update => delete)", () => {
            // create flow
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "0",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                initialFlowRate,    // flowRate
                BIG_INT_ZERO,       // previousSenderFlowRate
                BIG_INT_ZERO,       // previousReceiverFlowRate
                true                // isListed
            );

            // update flow: increase flow rate
            const increasedFlowRate = initialFlowRate.plus(BigInt.fromI32(50));
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "1",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                increasedFlowRate,  // flowRate
                initialFlowRate,    // previousSenderFlowRate
                initialFlowRate,    // previousReceiverFlowRate
                true                // isListed
            );


            // delete flow
            _modifyFlowAndAssertFlowUpdatedEventProperties(
                maticXAddress,      // superToken
                maticXName,         // tokenName
                maticXSymbol,       // tokenSymbol
                alice,              // sender
                bob,                // receiver
                ZERO_ADDRESS,       // underlyingToken
                "2",                // expectedType
                BIG_INT_ZERO,       // expectedOwedDeposit
                BIG_INT_ZERO,       // flowRate
                increasedFlowRate,  // previousSenderFlowRate
                increasedFlowRate,  // previousReceiverFlowRate
                true                // isListed
            );
        });

        test("handleFlowOperatorUpdated() - Should create a new FlowOperatorUpdatedEvent entity", () => {
            const superToken = maticXAddress;
            const permissions = 1; // create only
            const flowRateAllowance = BigInt.fromI32(100);
            const sender = alice;
            const flowOperator = bob;

            const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
                superToken,
                sender,
                flowOperator,
                permissions,
                flowRateAllowance
            );

            handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

            const id = assertEventBaseProperties(
                flowOperatorUpdatedEvent,
                "FlowOperatorUpdated"
            );
            const flowOperatorId = getFlowOperatorId(flowOperator, superToken, sender);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "token", superToken);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "sender", sender);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "flowOperator", flowOperatorId);
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "permissions", permissions.toString());
            assert.fieldEquals("FlowOperatorUpdatedEvent", id, "flowRateAllowance", flowRateAllowance.toString());
        });
    });

    describe("Higher Order Entity Mapping Tests", () => {
        beforeEach(() => {
            clearStore();
        });

        test("handleFlowOperatorUpdated() - Should create a new FlowOperator entity", () => {
            const superToken = maticXAddress;
            const permissions = 1; // create only
            const flowRateAllowance = BigInt.fromI32(100);
            const sender = alice;
            const flowOperator = bob;

            const flowOperatorUpdatedEvent = createFlowOperatorUpdatedEvent(
                superToken,
                sender,
                flowOperator,
                permissions,
                flowRateAllowance
            );

            handleFlowOperatorUpdated(flowOperatorUpdatedEvent);

            const id = getFlowOperatorId(flowOperator, superToken, sender);
            const atsId = getAccountTokenSnapshotID(Address.fromString(sender), Address.fromString(superToken));
            assertHigherOrderBaseProperties(
                "FlowOperator",
                id,
                flowOperatorUpdatedEvent.block.timestamp,
                flowOperatorUpdatedEvent.block.number,
                flowOperatorUpdatedEvent.block.timestamp,
                flowOperatorUpdatedEvent.block.number
            );
            assert.fieldEquals("FlowOperator", id, "permissions", permissions.toString());
            assert.fieldEquals("FlowOperator", id, "flowRateAllowanceGranted", flowRateAllowance.toString());
            assert.fieldEquals("FlowOperator", id, "flowRateAllowanceRemaining", flowRateAllowance.toString());
            assert.fieldEquals("FlowOperator", id, "flowOperator", flowOperator);
            assert.fieldEquals("FlowOperator", id, "sender", sender);
            assert.fieldEquals("FlowOperator", id, "token", superToken);
            assert.fieldEquals("FlowOperator", id, "accountTokenSnapshot", atsId);
        });
    });
});

/**
 * Create a flowUpdated event and assert the properties were created correctly
 * @param superToken 
 * @param tokenName 
 * @param tokenSymbol 
 * @param sender 
 * @param receiver 
 * @param underlyingToken 
 * @param expectedType 0 (create), 1 (update) or 2 (delete)
 * @param expectedOwedDeposit 
 * @param flowRate 
 * @param previousSenderFlowRate 
 * @param previousReceiverFlowRate 
 * @param isListed 
 * @returns FlowUpdated event
 */
function _modifyFlowAndAssertFlowUpdatedEventProperties(
    superToken: string,
    tokenName: string,
    tokenSymbol: string,
    sender: string,
    receiver: string,
    underlyingToken: Address,
    expectedType: string,
    expectedOwedDeposit: BigInt,
    flowRate: BigInt,
    previousSenderFlowRate: BigInt,
    previousReceiverFlowRate: BigInt,
    isListed: boolean
): FlowUpdated {
    const oldFlowRate = previousSenderFlowRate.abs();
    const flowRateDelta = flowRate.minus(previousSenderFlowRate);
    const totalSenderFlowRate = previousSenderFlowRate.minus(flowRateDelta);
    const totalReceiverFlowRate = previousReceiverFlowRate.plus(flowRateDelta);
    const userData = stringToBytes("");

    // NOTE: We ignore clipping here and only update flow: multiply flow rate by the liquidation period.
    const deposit = flowRate.times(LIQUIDATION_PERIOD);

    const flowUpdatedEvent = createFlowUpdatedEvent(
        superToken,
        sender,
        receiver,
        flowRate,
        totalSenderFlowRate,
        totalReceiverFlowRate,
        userData
    );

    mockedHandleFlowUpdatedRPCCalls(
        flowUpdatedEvent,
        superToken,
        DEFAULT_DECIMALS,
        tokenName,
        tokenSymbol,
        underlyingToken,
        deposit,
        expectedOwedDeposit,
        isListed
    );

    handleFlowUpdated(flowUpdatedEvent);

    const id = assertEventBaseProperties(
        flowUpdatedEvent,
        "FlowUpdated"
    );
    const streamId = getStreamID(
        Address.fromString(sender),
        Address.fromString(receiver),
        Address.fromString(superToken),
        0
    );
    assert.fieldEquals("FlowUpdatedEvent", id, "token", superToken);
    assert.fieldEquals("FlowUpdatedEvent", id, "sender", sender);
    assert.fieldEquals("FlowUpdatedEvent", id, "receiver", receiver);
    assert.fieldEquals("FlowUpdatedEvent", id, "flowOperator", ZERO_ADDRESS.toHexString());
    assert.fieldEquals("FlowUpdatedEvent", id, "flowRate", flowRate.toString());
    assert.fieldEquals("FlowUpdatedEvent", id, "totalSenderFlowRate", totalSenderFlowRate.toString());
    assert.fieldEquals("FlowUpdatedEvent", id, "totalReceiverFlowRate", totalReceiverFlowRate.toString());
    assert.fieldEquals("FlowUpdatedEvent", id, "deposit", deposit.toString());
    assert.fieldEquals("FlowUpdatedEvent", id, "userData", userData.toHexString());
    assert.fieldEquals("FlowUpdatedEvent", id, "oldFlowRate", oldFlowRate.toString());
    assert.fieldEquals("FlowUpdatedEvent", id, "type", expectedType);
    assert.fieldEquals("FlowUpdatedEvent", id, "stream", streamId);

    return flowUpdatedEvent;
}
