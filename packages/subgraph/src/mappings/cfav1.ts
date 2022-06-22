import { BigInt } from "@graphprotocol/graph-ts";
import {
    FlowOperatorUpdated,
    FlowUpdated,
    FlowUpdatedExtension,
    IConstantFlowAgreementV1,
} from "../../generated/ConstantFlowAgreementV1/IConstantFlowAgreementV1";
import {
    FlowOperatorUpdatedEvent,
    FlowUpdatedEvent,
    StreamPeriod,
    StreamRevision,
} from "../../generated/schema";
import {
    BIG_INT_ONE,
    BIG_INT_ZERO,
    bytesToAddress,
    createEventID,
    getFlowOperatorID,
    getOrder,
    getStreamPeriodID,
    MAX_FLOW_RATE,
    tokenHasValidHost,
    ZERO_ADDRESS,
} from "../utils";
import {
    createAccountTokenSnapshotLogEntity,
    createTokenStatisticLogEntity,
    getOrInitFlowOperator,
    getOrInitStream,
    getOrInitStreamRevision,
    updateAggregateEntitiesStreamData,
    updateATSStreamedAndBalanceUntilUpdatedAt,
} from "../mappingHelpers";
import { getHostAddress } from "../addresses";

enum FlowActionType {
    create,
    update,
    terminate,
}

function createFlowUpdatedEntity(
    event: FlowUpdated,
    oldFlowRate: BigInt,
    streamId: string,
    totalAmountStreamedUntilTimestamp: BigInt,
    deposit: BigInt
): FlowUpdatedEvent {
    let ev = new FlowUpdatedEvent(createEventID("FlowUpdated", event));
    ev.transactionHash = event.transaction.hash;
    ev.name = "FlowUpdated";
    ev.addresses = [
        event.params.token,
        event.params.sender,
        event.params.receiver,
    ];
    ev.timestamp = event.block.timestamp;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.token = event.params.token;
    ev.sender = event.params.sender;
    ev.receiver = event.params.receiver;
    ev.flowRate = event.params.flowRate;
    ev.totalSenderFlowRate = event.params.totalSenderFlowRate;
    ev.totalReceiverFlowRate = event.params.totalReceiverFlowRate;
    ev.userData = event.params.userData;
    ev.oldFlowRate = oldFlowRate;
    ev.stream = streamId;
    ev.totalAmountStreamedUntilTimestamp = totalAmountStreamedUntilTimestamp;
    ev.flowOperator = ZERO_ADDRESS;
    ev.deposit = deposit;

    let type = getFlowActionType(oldFlowRate, event.params.flowRate);
    ev.type = type;
    ev.save();
    return ev;
}

function createFlowOperatorUpdatedEventEntity(
    event: FlowOperatorUpdated
): FlowOperatorUpdatedEvent {
    let ev = new FlowOperatorUpdatedEvent(
        createEventID("FlowOperatorUpdated", event)
    );
    ev.transactionHash = event.transaction.hash;
    ev.name = "FlowOperatorUpdated";
    ev.addresses = [
        event.params.token,
        event.params.sender,
        event.params.flowOperator,
    ];
    ev.timestamp = event.block.timestamp;
    ev.blockNumber = event.block.number;
    ev.logIndex = event.logIndex;
    ev.order = getOrder(event.block.number, event.logIndex);
    ev.token = event.params.token;
    ev.sender = event.params.sender;
    ev.permissions = event.params.permissions;
    ev.flowRateAllowance = event.params.flowRateAllowance;
    ev.flowOperator = getFlowOperatorID(
        event.params.flowOperator,
        event.params.token,
        event.params.sender
    );

    ev.save();
    return ev;
}

function getFlowActionType(
    oldFlowRate: BigInt,
    newFlowRate: BigInt
): FlowActionType {
    return oldFlowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.create
        : newFlowRate.equals(BIG_INT_ZERO)
        ? FlowActionType.terminate
        : FlowActionType.update;
}

function handleStreamPeriodUpdate(
    event: FlowUpdated,
    eventEntity: FlowUpdatedEvent,
    previousFlowRate: BigInt,
    streamId: string,
    newDeposit: BigInt
): void {
    let streamRevision = getOrInitStreamRevision(
        event.params.sender,
        event.params.receiver,
        event.params.token
    );
    let flowActionType = getFlowActionType(
        previousFlowRate,
        event.params.flowRate
    );
    let previousStreamPeriod = StreamPeriod.load(
        getStreamPeriodID(streamId, streamRevision.periodRevisionIndex)
    );
    switch (flowActionType) {
        case FlowActionType.create:
            startStreamPeriod(
                eventEntity.id,
                event,
                streamRevision,
                streamId,
                newDeposit
            );
            break;
        case FlowActionType.update:
            if (!previousStreamPeriod) {
                throw Error(
                    "Previous StreamPeriod not found for flow update action"
                );
            }
            endStreamPeriod(
                eventEntity.id,
                previousStreamPeriod as StreamPeriod,
                event,
                streamRevision,
                previousFlowRate
            );
            startStreamPeriod(
                eventEntity.id,
                event,
                streamRevision,
                streamId,
                newDeposit
            );
            break;
        case FlowActionType.terminate:
            if (!previousStreamPeriod) {
                throw Error(
                    "Previous StreamPeriod not found for flow terminate action"
                );
            }
            endStreamPeriod(
                eventEntity.id,
                previousStreamPeriod as StreamPeriod,
                event,
                streamRevision,
                previousFlowRate
            );
            break;
        default:
            throw "Unrecognized FlowActionType";
    }
}

function incrementPeriodRevisionIndex(streamRevision: StreamRevision): void {
    streamRevision.periodRevisionIndex = streamRevision.periodRevisionIndex + 1;
    streamRevision.save();
}

function startStreamPeriod(
    flowUpdatedEventId: string,
    event: FlowUpdated,
    streamRevision: StreamRevision,
    streamId: string,
    newDeposit: BigInt
): void {
    let streamPeriod = new StreamPeriod(
        getStreamPeriodID(streamId, streamRevision.periodRevisionIndex)
    );
    streamPeriod.sender = event.params.sender.toHex();
    streamPeriod.receiver = event.params.receiver.toHex();
    streamPeriod.token = event.params.token.toHex();
    streamPeriod.flowRate = event.params.flowRate;
    streamPeriod.startedAtTimestamp = event.block.timestamp;
    streamPeriod.startedAtBlockNumber = event.block.number;
    streamPeriod.startedAtEvent = flowUpdatedEventId;
    streamPeriod.stream = streamId;
    streamPeriod.deposit = newDeposit;
    streamPeriod.save();
}

function endStreamPeriod(
    flowUpdatedEventId: string,
    existingStreamPeriod: StreamPeriod,
    event: FlowUpdated,
    streamRevision: StreamRevision,
    flowRateBeforeUpdate: BigInt
): void {
    let streamStopTime = event.block.timestamp;
    existingStreamPeriod.stoppedAtTimestamp = streamStopTime;
    existingStreamPeriod.stoppedAtBlockNumber = event.block.number;
    existingStreamPeriod.stoppedAtEvent = flowUpdatedEventId;
    existingStreamPeriod.totalAmountStreamed = flowRateBeforeUpdate.times(
        streamStopTime.minus(existingStreamPeriod.startedAtTimestamp)
    );
    existingStreamPeriod.save();
    incrementPeriodRevisionIndex(streamRevision);
}

export function handleStreamUpdated(event: FlowUpdated): void {
    let senderAddress = event.params.sender;
    let receiverAddress = event.params.receiver;
    let tokenAddress = event.params.token;
    let flowRate = event.params.flowRate;
    let currentTimestamp = event.block.timestamp;
    let hostAddress = getHostAddress();

    let hasValidHost = tokenHasValidHost(hostAddress, tokenAddress);
    if (!hasValidHost) {
        return;
    }

    // create or update stream entity
    let cfaContract = IConstantFlowAgreementV1.bind(event.address);
    let depositResult = cfaContract.try_getFlow(
        event.params.token,
        event.params.sender,
        event.params.receiver
    );

    // NOTE: if we want to optimize this in the future,
    // we have to create a new global entity which is
    // updated once we first see the FlowUpdatedExtension event
    // and once we see it, we rely on the handleFlowUpdatedExtension
    // handler to get us the deposit instead of doing a web3 query
    // every time
    let newDeposit = depositResult.reverted
        ? BigInt.fromI32(0)
        : depositResult.value.value2; // deposit

    let stream = getOrInitStream(event);
    let oldDeposit = stream.deposit;
    let oldFlowRate = stream.currentFlowRate;

    let timeSinceLastUpdate = currentTimestamp.minus(stream.updatedAtTimestamp);
    let userAmountStreamedSinceLastUpdate =
        oldFlowRate.times(timeSinceLastUpdate);
    let newStreamedUntilLastUpdate = stream.streamedUntilUpdatedAt.plus(
        userAmountStreamedSinceLastUpdate
    );
    let depositDelta = newDeposit.minus(oldDeposit);
    stream.currentFlowRate = flowRate;
    stream.streamedUntilUpdatedAt = newStreamedUntilLastUpdate;
    stream.updatedAtTimestamp = currentTimestamp;
    stream.updatedAtBlockNumber = event.block.number;
    stream.deposit = newDeposit;
    stream.save();

    let flowRateDelta = flowRate.minus(oldFlowRate);
    let isCreate = oldFlowRate.equals(BIG_INT_ZERO);
    let isDelete = flowRate.equals(BIG_INT_ZERO);
    if (isDelete) {
        let streamRevision = getOrInitStreamRevision(
            senderAddress,
            receiverAddress,
            tokenAddress
        );
        streamRevision.revisionIndex = streamRevision.revisionIndex + 1;
        streamRevision.save();
    }

    // create event entity
    let flowUpdateEvent = createFlowUpdatedEntity(
        event,
        oldFlowRate,
        stream.id,
        newStreamedUntilLastUpdate,
        newDeposit
    );
    handleStreamPeriodUpdate(
        event,
        flowUpdateEvent,
        oldFlowRate,
        stream.id,
        newDeposit
    );

    updateATSStreamedAndBalanceUntilUpdatedAt(
        senderAddress,
        tokenAddress,
        event.block,
        // @note when deleting, we do RPC call (prevents double accounting post-liquidation)
        isDelete ? null : BIG_INT_ZERO.minus(depositDelta)
    );
    updateATSStreamedAndBalanceUntilUpdatedAt(
        receiverAddress,
        tokenAddress,
        event.block,
        BIG_INT_ZERO
    );
    // @note EXCEPTION for not calling updateTokenStatsStreamedUntilUpdatedAt
    // because updateAggregateEntitiesStreamData updates tokenStats.streamedUntilUpdatedAt
    updateAggregateEntitiesStreamData(
        senderAddress,
        receiverAddress,
        tokenAddress,
        flowRate,
        flowRateDelta,
        depositDelta,
        isCreate,
        isDelete,
        event.block
    );
    createAccountTokenSnapshotLogEntity(
        event,
        senderAddress,
        tokenAddress,
        "FlowUpdated"
    );
    createAccountTokenSnapshotLogEntity(
        event,
        receiverAddress,
        tokenAddress,
        "FlowUpdated"
    );
    createTokenStatisticLogEntity(event, tokenAddress, "FlowUpdated");
}

// NOTE: This handler is run right after handleStreamUpdated as the FlowUpdatedExtension
// event is emitted right after the FlowUpdated event in the contract. We update the
// FlowUpdated event to include the flowOperator and deposit properties.
// Given that we know that the events occur right after each other in the same transaction
// we have the neccesary information to load the FlowUpdated entity to update it.
export function handleFlowUpdatedExtension(event: FlowUpdatedExtension): void {
    let previousLogIndex = event.logIndex.minus(BIG_INT_ONE);
    let flowUpdatedEvent = FlowUpdatedEvent.load(
        "FlowUpdated-" +
            event.transaction.hash.toHexString() +
            "-" +
            previousLogIndex.toString()
    );
    if (flowUpdatedEvent != null) {
        flowUpdatedEvent.flowOperator = event.params.flowOperator;
        flowUpdatedEvent.deposit = event.params.deposit;
        flowUpdatedEvent.addresses = flowUpdatedEvent.addresses.concat([
            event.params.flowOperator,
        ]);
        flowUpdatedEvent.save();

        // we know that the flowOperator should be updated if the flowOperator is
        // not the sender - it doesn't matter when they delete as we don't need to
        // update anything in that scenario
        if (flowUpdatedEvent.flowOperator.notEqual(flowUpdatedEvent.sender)) {
            updateFlowOperatorForFlowUpdated(event, flowUpdatedEvent);
        }
    }
}

export function handleFlowOperatorUpdated(event: FlowOperatorUpdated): void {
    let flowOperator = getOrInitFlowOperator(
        event.block,
        event.params.flowOperator,
        event.params.token,
        event.params.sender
    );

    flowOperator.permissions = event.params.permissions;
    flowOperator.flowRateAllowanceGranted = event.params.flowRateAllowance;
    flowOperator.flowRateAllowanceRemaining = event.params.flowRateAllowance;
    flowOperator.flowOperator = event.params.flowOperator;
    flowOperator.save();

    createFlowOperatorUpdatedEventEntity(event);
}

export function updateFlowOperatorForFlowUpdated(
    event: FlowUpdatedExtension,
    flowUpdatedEvent: FlowUpdatedEvent
): void {
    if (flowUpdatedEvent.type == FlowActionType.terminate) {
        return;
    }
    let flowOperator = getOrInitFlowOperator(
        event.block,
        event.params.flowOperator,
        bytesToAddress(flowUpdatedEvent.token),
        bytesToAddress(flowUpdatedEvent.sender)
    );

    if (flowUpdatedEvent.type == FlowActionType.create) {
        flowOperator.flowRateAllowanceRemaining =
            flowOperator.flowRateAllowanceGranted.equals(MAX_FLOW_RATE)
                ? flowOperator.flowRateAllowanceGranted
                : flowOperator.flowRateAllowanceRemaining.minus(
                      flowUpdatedEvent.flowRate
                  );
    }

    if (flowUpdatedEvent.type == FlowActionType.update) {
        let flowRateAllowanceDelta = flowUpdatedEvent.flowRate.minus(
            flowUpdatedEvent.oldFlowRate
        );
        flowOperator.flowRateAllowanceRemaining =
            flowOperator.flowRateAllowanceGranted.equals(MAX_FLOW_RATE) ||
            flowRateAllowanceDelta.lt(BIG_INT_ZERO)
                ? flowOperator.flowRateAllowanceGranted
                : flowOperator.flowRateAllowanceRemaining.minus(
                      flowRateAllowanceDelta
                  );
    }
    flowOperator.save();
}
