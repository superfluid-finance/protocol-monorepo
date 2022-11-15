import { expect } from "chai";
import {
    actionTypeToPeriodRevisionIndexDeltaMap,
    FlowActionType,
} from "../../helpers/constants";
import { fetchEntityAndEnsureExistence, toBN } from "../../helpers/helpers";
import { IStream, IStreamData, IEvent, IStreamPeriod } from "../../interfaces";
import { getStream, getStreamPeriod } from "../../queries/holQueries";
import { validateReverseLookup } from "../validators";

export const fetchStreamPeriodAndValidate = async (
    streamData: IStreamData,
    flowRate: string,
    event: IEvent,
    actionType: FlowActionType,
    newDeposit: string
) => {
    if (actionType === FlowActionType.Create) {
        await validateStartedStreamPeriod(
            streamData,
            flowRate,
            event,
            actionType,
            newDeposit
        );
    } else if (actionType === FlowActionType.Update) {
        await validateClosedStreamPeriod(streamData, event);
        await validateStartedStreamPeriod(
            streamData,
            flowRate,
            event,
            actionType,
            newDeposit
        );
    } else if (actionType === FlowActionType.Delete) {
        await validateClosedStreamPeriod(streamData, event);
    }
};
const validateClosedStreamPeriod = async (
    pastStreamData: IStreamData,
    event: IEvent
) => {
    const streamPeriodId =
        pastStreamData.id +
        "-" +
        Number(pastStreamData.periodRevisionIndex).toFixed(1);
    const { streamPeriod, stream } = await fetchStreamPeriodWithStream(
        streamPeriodId
    );
    const expectedAmountStreamed = toBN(pastStreamData.oldFlowRate).mul(
        toBN(streamPeriod.stoppedAtTimestamp).sub(
            streamPeriod.startedAtTimestamp
        )
    );
    validateStreamPeriodFromStream(streamPeriod, stream);

    expect(
        streamPeriod.stoppedAtEvent.id,
        "StreamPeriod: stoppedAtEvent error"
    ).to.equal(event.id);
    expect(
        streamPeriod.stoppedAtTimestamp,
        "StreamPeriod: stoppedAtTimestamp error"
    ).to.equal(event.timestamp);
    expect(
        streamPeriod.stoppedAtBlockNumber,
        "StreamPeriod: stoppedAtBlockNumber error"
    ).to.equal(event.blockNumber);
    expect(streamPeriod.flowRate, "StreamPeriod: flowRate error").to.equal(
        pastStreamData.oldFlowRate
    );
    expect(
        streamPeriod.totalAmountStreamed,
        "StreamPeriod: totalAmountStreamed error"
    ).to.equal(expectedAmountStreamed);
};

const validateStartedStreamPeriod = async (
    pastStreamData: IStreamData,
    flowRate: string,
    event: IEvent,
    actionType: FlowActionType,
    newDeposit: string
) => {
    const newPeriodRevisionIndex =
        Number(pastStreamData.periodRevisionIndex) +
        (actionTypeToPeriodRevisionIndexDeltaMap.get(actionType) || 0);

    const streamPeriodId =
        pastStreamData.id + "-" + newPeriodRevisionIndex.toFixed(1);

    const { streamPeriod, stream } = await fetchStreamPeriodWithStream(
        streamPeriodId
    );
    validateStreamPeriodFromStream(streamPeriod, stream);

    expect(
        streamPeriod.startedAtEvent.id,
        "StreamPeriod: startedAtEvent error"
    ).to.equal(event.id);
    expect(streamPeriod.deposit, "StreamPeriod: deposit error").to.equal(
        newDeposit
    );
    expect(
        streamPeriod.startedAtTimestamp,
        "StreamPeriod: startedAtTimestamp error"
    ).to.equal(event.timestamp);
    expect(
        streamPeriod.startedAtBlockNumber,
        "StreamPeriod: startedAtBlockNumber error"
    ).to.equal(event.blockNumber);
    expect(streamPeriod.flowRate, "StreamPeriod: flowRate error").to.equal(
        flowRate
    );
    validateReverseLookup(streamPeriod, stream.streamPeriods);
};

const validateStreamPeriodFromStream = (
    streamPeriod: IStreamPeriod,
    stream: IStream
) => {
    expect(streamPeriod.stream.id, "StreamPeriod: stream error").to.equal(
        stream.id
    );
    expect(streamPeriod.receiver.id, "StreamPeriod: receiver error").to.equal(
        stream.receiver.id
    );
    expect(streamPeriod.sender.id, "StreamPeriod: sender error").to.equal(
        stream.sender.id
    );
    expect(streamPeriod.token.id, "StreamPeriod: token error").to.equal(
        stream.token.id
    );
};

async function fetchStreamPeriodWithStream(streamPeriodId: string) {
    const streamPeriod = await fetchEntityAndEnsureExistence<IStreamPeriod>(
        getStreamPeriod,
        streamPeriodId,
        "StreamPeriod"
    );
    const stream = await fetchEntityAndEnsureExistence<IStream>(
        getStream,
        streamPeriod.stream.id,
        "Stream"
    );
    return { streamPeriod, stream };
}
