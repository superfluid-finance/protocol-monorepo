import { expect } from "chai";
import { IStream } from "../interfaces";


export const validateStreamEntity = (
    subgraphStream: IStream,
    flowedAmountSinceUpdatedAt: number,
    streamId: string,
    receiverFlowRate: string
) => {
    const expectedStreamedUntilUpdatedAt =
        Number(subgraphStream.streamedUntilUpdatedAt) +
        flowedAmountSinceUpdatedAt;

    expect(subgraphStream.id).to.be.equal(streamId);
    expect(subgraphStream.currentFlowRate).to.equal(receiverFlowRate);
    expect(subgraphStream.streamedUntilUpdatedAt).to.be.equal(
        expectedStreamedUntilUpdatedAt.toString()
    );
};