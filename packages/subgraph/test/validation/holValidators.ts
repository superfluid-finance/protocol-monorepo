import { BigNumber } from "@ethersproject/bignumber";
import { expect } from "chai";
import { ethers } from "ethers";
import { InstantDistributionAgreementV1 } from "../../typechain/InstantDistributionAgreementV1";
import { subgraphRequest, toBN } from "../helpers/helpers";
import { IIndex, IStream, IStreamData, ISubscription } from "../interfaces";
import { getIndex, getStream, getSubscription } from "../queries/holQueries";

export const fetchStreamAndValidate = async (
    streamData: IStreamData,
    expectedStreamedUntilUpdatedAt: BigNumber,
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

export const fetchSubscriptionAndValidate = async (
    idaV1: InstantDistributionAgreementV1,
    expectedSubscription: ISubscription,
    newIndexValue: string
) => {
    const { indexSubscription } = await subgraphRequest<{
        indexSubscription: ISubscription | undefined;
    }>(getSubscription, {
        id: expectedSubscription.id,
    });

    if (!indexSubscription) {
        throw new Error("Subscription entity not found.");
    }

    validateSubscriptionEntity(
        idaV1,
        indexSubscription,
        expectedSubscription,
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
    expect(subgraphIndex.oldIndexValue, "Index: oldIndexValue error").to.equal(
        expectedIndex.oldIndexValue
    );
    expect(subgraphIndex.newIndexValue, "Index: newIndexValue error").to.equal(
        expectedIndex.newIndexValue
    );
    expect(
        subgraphIndex.totalSubscriptions,
        "Index: totalSubscriptions error"
    ).to.equal(expectedIndex.totalSubscriptions);
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

export const validateSubscriptionEntity = async (
    idaV1: InstantDistributionAgreementV1,
    subgraphSubscription: ISubscription,
    expectedSubscription: ISubscription,
    newIndexValue: string
) => {
    const token = ethers.utils.getAddress(subgraphSubscription.token.id);
    const publisher = ethers.utils.getAddress(
        subgraphSubscription.publisher.id
    );
    const subscriberAddress = ethers.utils.getAddress(
        subgraphSubscription.subscriber.id
    );

    const [, approved, units, pendingDistribution] =
        await idaV1.getSubscription(
            token,
            publisher,
            Number(subgraphSubscription.indexId),
            subscriberAddress
        );

    // Check subgraph data against expected data
    expect(
        subgraphSubscription.indexId,
        "Subscription: indexId error"
    ).to.equal(expectedSubscription.indexId);
    expect(
        subgraphSubscription.approved,
        "Subscription: approved error"
    ).to.equal(expectedSubscription.approved);
    expect(subgraphSubscription.units, "Subscription: units error").to.equal(
        expectedSubscription.units
    );
    expect(
        subgraphSubscription.totalAmountReceivedUntilUpdatedAt,
        "Subscription: totalAmountReceivedUntilUpdatedAt error"
    ).to.equal(expectedSubscription.totalAmountReceivedUntilUpdatedAt);
    expect(
        subgraphSubscription.lastIndexValue,
        "Subscription: lastIndexValue error"
    ).to.equal(expectedSubscription.lastIndexValue);

    // Check subgraph data against web3 data
    expect(
        subgraphSubscription.approved,
        "Subscription: approved error"
    ).to.equal(approved);
    expect(subgraphSubscription.units, "Subscription: units error").to.equal(
        units.toString()
    );
    const calcPendingDistribution = approved
        ? "0"
        : toBN(subgraphSubscription.units).mul(
              toBN(newIndexValue).sub(toBN(subgraphSubscription.lastIndexValue))
          );
    expect(calcPendingDistribution.toString()).to.equal(
        pendingDistribution.toString()
    );
};
