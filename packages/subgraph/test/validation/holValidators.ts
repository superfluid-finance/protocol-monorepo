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
    expect(subgraphIndex.indexId, "Index: indexId error").to.equal(
        expectedIndex.indexId
    );
    expect(subgraphIndex.userData, "Index: userData error").to.equal(
        expectedIndex.userData
    );
    expect(subgraphIndex.oldIndexValue, "Index: oldIndexValue error").to.equal(
        expectedIndex.oldIndexValue
    );
    expect(subgraphIndex.newIndexValue, "Index: newIndexValue error").to.equal(
        expectedIndex.newIndexValue
    );
    expect(
        subgraphIndex.totalSubscribers,
        "Index: totalSubscribers error"
    ).to.equal(expectedIndex.totalSubscribers);
    expect(
        subgraphIndex.totalUnitsPending,
        "Index: totalUnitsPending error"
    ).to.equal(expectedIndex.totalUnitsPending);
    expect(
        subgraphIndex.totalUnitsApproved,
        "Index: totalUnitsApproved error"
    ).to.equal(expectedIndex.totalUnitsApproved);
    expect(subgraphIndex.totalUnits, "Index: totalUnits error").to.equal(
        expectedIndex.totalUnits
    );
    expect(
        subgraphIndex.totalAmountDistributedUntilUpdatedAt,
        "Index: totalAmountDistributedUntilUpdatedAt error"
    ).to.equal(expectedIndex.totalAmountDistributedUntilUpdatedAt);

    // Check subgraph data against web3 data
    const totalUnits = totalUnitsPending.add(totalUnitsApproved);
    expect(subgraphIndex.newIndexValue, "Index: newIndexValue error").to.equal(
        indexValue.toString()
    );
    expect(
        subgraphIndex.totalUnitsApproved,
        "Index: totalUnitsApproved error"
    ).to.equal(totalUnitsApproved.toString());
    expect(
        subgraphIndex.totalUnitsPending,
        "Index: totalUnitsPending error"
    ).to.equal(totalUnitsPending);
    expect(subgraphIndex.totalUnits, "Index: totalUnits error").to.equal(
        totalUnits.toString()
    );
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
    expect(subgraphSubscriber.indexId, "Subscriber: indexId error").to.equal(
        expectedSubscriber.indexId
    );
    expect(subgraphSubscriber.userData, "Subscriber: userData error").to.equal(
        expectedSubscriber.userData
    );
    expect(subgraphSubscriber.approved, "Subscriber: approved error").to.equal(
        expectedSubscriber.approved
    );
    expect(subgraphSubscriber.units, "Subscriber: units error").to.equal(
        expectedSubscriber.units
    );
    expect(
        subgraphSubscriber.totalAmountReceivedUntilUpdatedAt,
        "Subscriber: totalAmountReceivedUntilUpdatedAt error"
    ).to.equal(expectedSubscriber.totalAmountReceivedUntilUpdatedAt);
    expect(
        subgraphSubscriber.lastIndexValue,
        "Subscriber: lastIndexValue error"
    ).to.equal(expectedSubscriber.lastIndexValue);

    // Check subgraph data against web3 data
    expect(subgraphSubscriber.approved, "Subscriber: approved error").to.equal(
        approved
    );
    expect(subgraphSubscriber.units, "Subscriber: units error").to.equal(
        units.toString()
    );
    const calcPendingDistribution = approved
        ? "0"
        : toBN(subgraphSubscriber.units).mul(
              toBN(newIndexValue).sub(toBN(subgraphSubscriber.lastIndexValue))
          );
    expect(calcPendingDistribution.toString()).to.equal(
        pendingDistribution.toString()
    );
};
