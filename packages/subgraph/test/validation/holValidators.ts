import { BigNumber } from "@ethersproject/bignumber";
import { InstantDistributionAgreementV1Helper } from "@superfluid-finance/js-sdk/src/InstantDistributionAgreementV1Helper";
import { expect } from "chai";
import { ethers } from "ethers";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { subgraphRequest, toBN } from "../helpers/helpers";
import { IIndex, IStream, IStreamData, ISubscriber } from "../interfaces";
import { getIndex, getStream, getSubscriber } from "../queries/holQueries";

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

    const expectedStreamedUntilUpdatedAt = toBN(streamedUntilUpdatedAt).add(
        streamedAmountSinceUpdatedAt
    );

    validateStreamEntity(
        stream,
        expectedStreamedUntilUpdatedAt.toString(),
        streamId,
        flowRate
    );
};

export const fetchIndexAndValidate = async (expectedIndex: IIndex) => {
    const { index } = await subgraphRequest<{ index: IIndex | undefined }>(
        getIndex,
        {
            id: expectedIndex.id,
        }
    );
};

export const fetchSubscriberAndValidate = async (
    expectedSubscriber: ISubscriber
) => {
    const { subscriber } = await subgraphRequest<{
        subscriber: ISubscriber | undefined;
    }>(getSubscriber, {
        id: expectedSubscriber.id,
    });
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

export const validateIndexEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphIndex: IIndex,
    expectedIndex: IIndex
) => {
    const superToken = ethers.utils.getAddress(subgraphIndex.token.id);
    const publisher = ethers.utils.getAddress(subgraphIndex.publisher.id);
    const [, indexValue, totalUnitsApproved, totalUnitsPending] =
        await idaV1.getIndex(superToken, publisher, Number(expectedIndex.indexId));

	// Check subgraph data against expected data
    expect(subgraphIndex.indexId).to.equal(expectedIndex.indexId);
    expect(subgraphIndex.userData).to.equal(expectedIndex.userData);
    expect(subgraphIndex.oldIndexValue).to.equal(expectedIndex.oldIndexValue);
    expect(subgraphIndex.newIndexValue).to.equal(expectedIndex.newIndexValue);
    expect(subgraphIndex.totalSubscribers).to.equal(
        expectedIndex.totalSubscribers
    );
    expect(subgraphIndex.totalUnitsPending).to.equal(
        expectedIndex.totalUnitsPending
    );
    expect(subgraphIndex.totalUnitsApproved).to.equal(
        expectedIndex.totalUnitsApproved
    );
    expect(subgraphIndex.totalUnits).to.equal(expectedIndex.totalUnits);
    expect(subgraphIndex.totalUnitsDistributedUntilUpdatedAt).to.equal(
        expectedIndex.totalUnitsDistributedUntilUpdatedAt
    );
	
	// Check subgraph data against web3 data
};

export const validateSubscriberEntity = async (
    subgraphSubscriber: ISubscriber,
    expectedSubscriber: ISubscriber
) => {
    expect(subgraphSubscriber.indexId).to.equal(expectedSubscriber.indexId);
};
