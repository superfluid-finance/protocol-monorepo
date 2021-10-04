import { BigNumber } from "@ethersproject/bignumber";
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

export const fetchIndexAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedIndex: IIndex
) => {
    const { index } = await subgraphRequest<{ index: IIndex | undefined }>(
        getIndex,
        {
            id: expectedIndex.id,
        }
    );

    if (!index) {
        throw new Error("Index entity not found.");
    }

    validateIndexEntity(idaV1, index, expectedIndex);
};

export const fetchSubscriberAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedSubscriber: ISubscriber,
    newIndexValue: string
) => {
    const { subscriber } = await subgraphRequest<{
        subscriber: ISubscriber | undefined;
    }>(getSubscriber, {
        id: expectedSubscriber.id,
    });

    if (!subscriber) {
        throw new Error("Subscriber entity not found.");
    }

    validateSubscriberEntity(
        idaV1,
        subscriber,
        expectedSubscriber,
        newIndexValue
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

export const validateIndexEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphIndex: IIndex,
    expectedIndex: IIndex
) => {
    const superToken = ethers.utils.getAddress(subgraphIndex.token.id);
    const publisher = ethers.utils.getAddress(subgraphIndex.publisher.id);
    const [, indexValue, totalUnitsApproved, totalUnitsPending] =
        await idaV1.getIndex(
            superToken,
            publisher,
            Number(expectedIndex.indexId)
        );

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
    expect(subgraphIndex.totalAmountDistributedUntilUpdatedAt).to.equal(
        expectedIndex.totalAmountDistributedUntilUpdatedAt
    );

    // Check subgraph data against web3 data
    const totalUnits = totalUnitsPending.add(totalUnitsApproved);
    expect(subgraphIndex.newIndexValue).to.equal(indexValue.toString());
    expect(subgraphIndex.totalUnitsApproved).to.equal(
        totalUnitsApproved.toString()
    );
    expect(subgraphIndex.totalUnitsPending).to.equal(totalUnitsPending);
    expect(subgraphIndex.totalUnits).to.equal(totalUnits.toString());
};

export const validateSubscriberEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphSubscriber: ISubscriber,
    expectedSubscriber: ISubscriber,
    newIndexValue: string
) => {
    const token = ethers.utils.getAddress(subgraphSubscriber.token.id);
    const publisher = ethers.utils.getAddress(subgraphSubscriber.publisher.id);
    const subscriber = ethers.utils.getAddress(
        subgraphSubscriber.subscriber.id
    );

    const [, approved, units, pendingDistribution] =
        await idaV1.getSubscription(
            token,
            publisher,
            Number(subgraphSubscriber.indexId),
            subscriber
        );

    // Check subgraph data against expected data
    expect(subgraphSubscriber.indexId).to.equal(expectedSubscriber.indexId);
    expect(subgraphSubscriber.userData).to.equal(expectedSubscriber.userData);
    expect(subgraphSubscriber.approved).to.equal(expectedSubscriber.approved);
    expect(subgraphSubscriber.units).to.equal(expectedSubscriber.units);
    expect(subgraphSubscriber.totalAmountReceivedUntilUpdatedAt).to.equal(
        expectedSubscriber.totalAmountReceivedUntilUpdatedAt
    );
    expect(subgraphSubscriber.lastIndexValue).to.equal(
        expectedSubscriber.lastIndexValue
    );

    // Check subgraph data against web3 data
    expect(subgraphSubscriber.approved).to.equal(approved);
    expect(subgraphSubscriber.units).to.equal(units.toString());
    const calcPendingDistribution = approved
        ? 0
        : toBN(newIndexValue)
              .sub(subgraphSubscriber.lastIndexValue)
              .mul(subgraphSubscriber.units);
    expect(calcPendingDistribution).to.equal(pendingDistribution);
};
