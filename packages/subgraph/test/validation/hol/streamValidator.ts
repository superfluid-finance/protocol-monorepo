import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { fetchEntityAndEnsureExistence } from "../../helpers/helpers";
import { IStream, IStreamData, IEvent, IAccount } from "../../interfaces";
import { getAccount, getStream } from "../../queries/holQueries";
import { validateReverseLookup } from "../validators";

export const fetchStreamAndValidate = async (
    streamData: IStreamData,
    expectedStreamedUntilUpdatedAt: BigNumber,
    newFlowRate: string,
    event: IEvent,
    isCreate: boolean
) => {
    const streamId = streamData.id;
    const stream = await fetchEntityAndEnsureExistence<IStream>(
        getStream,
        streamId,
        "Stream"
    );

    validateStreamEntity(
        stream,
        expectedStreamedUntilUpdatedAt.toString(),
        streamId,
        newFlowRate
    );

    // validate flowUpdated reverse lookup on Stream entity
    validateReverseLookup(event, stream.flowUpdatedEvents);

    if (isCreate) {
        // validate accounts reverse lookup on Stream entity creation
        await validateAccountReverseLookupsForStream(stream);
    }
};

const validateAccountReverseLookupsForStream = async (stream: IStream) => {
    const senderAccount = await fetchEntityAndEnsureExistence<IAccount>(
        getAccount,
        stream.sender.id,
        "Account"
    );
    const receiverAccount = await fetchEntityAndEnsureExistence<IAccount>(
        getAccount,
        stream.receiver.id,
        "Account"
    );
    const streamLightEntity = { id: stream.id };
    validateReverseLookup(streamLightEntity, senderAccount.outflows);
    validateReverseLookup(streamLightEntity, receiverAccount.inflows);
};

const validateStreamEntity = (
    subgraphStream: IStream,
    expectedStreamedUntilUpdatedAt: string,
    streamId: string,
    newFlowRate: string
) => {
    expect(subgraphStream.id, "Stream: id error").to.equal(streamId);
    expect(
        subgraphStream.currentFlowRate,
        "Stream: currentFlowRate error"
    ).to.equal(newFlowRate);
    expect(
        subgraphStream.streamedUntilUpdatedAt,
        "Stream: streamedUntilUpdatedAt error"
    ).to.equal(expectedStreamedUntilUpdatedAt);
};
