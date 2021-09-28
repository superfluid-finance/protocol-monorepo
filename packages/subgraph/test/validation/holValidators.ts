import { expect } from "chai";
import { IStream } from "../interfaces";

export const validateStreamEntity = (
    subgraphStream: IStream,
    expectedStreamedUntilUpdatedAt: string,
    streamId: string,
    receiverFlowRate: string
) => {
    expect(subgraphStream.id, "Stream: id error").to.be.equal(streamId);
    expect(
        subgraphStream.currentFlowRate,
        "Stream: currentFlowRate error"
    ).to.equal(receiverFlowRate);
    expect(
        subgraphStream.streamedUntilUpdatedAt,
        "Stream: streamedUntilUpdatedAt error"
    ).to.be.equal(expectedStreamedUntilUpdatedAt);
};
