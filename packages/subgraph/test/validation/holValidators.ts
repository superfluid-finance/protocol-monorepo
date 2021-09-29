import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { subgraphRequest, toBN } from "../helpers/helpers";
import { IStream, IStreamData } from "../interfaces";
import { getStream } from "../queries/holQueries";

export const fetchStreamAndValidate = async (
    streamData: IStreamData,
    streamedAmountSinceUpdatedAt: BigNumber,
    flowRate: string
) => {
    const streamId = streamData.id;
    const { stream } = await subgraphRequest<{ stream: IStream | undefined }>(
        getStream,
        {
            id: streamId,
        }
    );

    if (!stream) {
        throw new Error("Stream entity not found.");
    }
    const { streamedUntilUpdatedAt } = streamData;

    const expectedStreamedUntilUpdatedAt =
        toBN(streamedUntilUpdatedAt).add(streamedAmountSinceUpdatedAt);

    validateStreamEntity(
        stream,
        expectedStreamedUntilUpdatedAt.toString(),
        streamId,
        flowRate
    );
};

export const validateStreamEntity = (
    subgraphStream: IStream,
    expectedStreamedUntilUpdatedAt: string,
    streamId: string,
    currentFlowRate: string
) => {
    expect(subgraphStream.id, "Stream: id error").to.be.equal(streamId);
    expect(
        subgraphStream.currentFlowRate,
        "Stream: currentFlowRate error"
    ).to.equal(currentFlowRate);
    expect(
        subgraphStream.streamedUntilUpdatedAt,
        "Stream: streamedUntilUpdatedAt error"
    ).to.be.equal(expectedStreamedUntilUpdatedAt);
};
