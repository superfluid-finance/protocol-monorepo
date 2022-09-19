import {BigNumber} from "ethers";
import {expect} from "hardhat";
import _ from "lodash";

import {toBN} from "../utils/helpers";

import {
    IDABaseParams,
    IDAIndexData,
    IDASubscriptionData,
    PartialIDAIndexData,
} from "./Agreement.types";

const {web3tx, wad4human} = require("@decentral.ee/web3-helpers");
const {expectEvent} = require("@openzeppelin/test-helpers");

function _updateIndexData({
    testenv,
    superToken,
    publisher,
    indexId,
    indexData,
}: IDABaseParams & {publisher: string; indexData: PartialIDAIndexData}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken.address]: {
                accounts: {
                    [publisher]: {
                        ida: {
                            indices: {
                                [indexId]: {
                                    data: indexData,
                                },
                            },
                        },
                    },
                },
            },
        },
    });
}

function _assertEqualIndexData(
    idataActual: IDAIndexData,
    idataExpected: IDAIndexData
) {
    expect(idataActual.exist).to.equal(idataExpected.exist);
    expect(idataActual.indexValue).to.equal(idataExpected.indexValue);
    expect(idataActual.totalUnitsApproved).to.equal(
        idataExpected.totalUnitsApproved
    );
    expect(idataActual.totalUnitsPending).to.equal(
        idataExpected.totalUnitsPending
    );
}

export function getIndexData({
    testenv,
    superToken,
    publisher,
    indexId,
}: IDABaseParams & {publisher: string}) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken.address]: {
                accounts: {
                    [publisher]: {
                        ida: {
                            indices: {
                                [indexId]: {
                                    data: {
                                        exist: true,
                                        indexValue: toBN(0),
                                        totalUnitsApproved: toBN(0),
                                        totalUnitsPending: toBN(0),
                                    },
                                    subscribers: {},
                                },
                            },
                        },
                    },
                },
            },
        },
    });
    return _.clone(
        testenv.data.tokens[superToken.address].accounts[publisher].ida.indices[
            indexId
        ].data
    );
}

export function getSubscribers({
    testenv,
    superToken,
    publisher,
    indexId,
}: IDABaseParams & {publisher: string}) {
    return testenv.data.tokens[superToken.address].accounts[publisher].ida
        .indices[indexId].subscribers;
}

function _updateSubscriptionData({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
    subscriptionData,
}: IDABaseParams & {
    subscriber: string;
    subscriptionData: IDASubscriptionData;
}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken.address]: {
                accounts: {
                    [subscriber]: {
                        ida: {
                            subscriptions: {
                                [`${publisher}@${indexId}`]: subscriptionData,
                            },
                        },
                    },
                },
            },
        },
    });
}

function _assertEqualSubscriptionData(
    sdataActual: IDASubscriptionData,
    sdataExpected: IDASubscriptionData
) {
    expect(sdataActual.exist).to.equal(sdataExpected.exist);
    expect(sdataActual.approved).to.equal(sdataExpected.approved);
    expect(sdataActual.units).to.equal(sdataExpected.units);
    expect(sdataActual.pendingDistribution).to.equal(
        sdataExpected.pendingDistribution
    );
}

function _deleteSubscription({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
}: IDABaseParams & {subscriber: string}) {
    delete testenv.data.tokens[superToken.address].accounts[subscriber].ida
        .subscriptions[`${publisher}@${indexId}`];
}

export function getSubscriptionData({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
}: IDABaseParams & {publisher: string; subscriber: string}) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken.address]: {
                accounts: {
                    [subscriber]: {
                        ida: {
                            subscriptions: {
                                [`${publisher}@${indexId}`]: {
                                    exist: false,
                                    approved: false,
                                    units: toBN(0),
                                    _syncedIndexValue: toBN(0),
                                },
                            },
                        },
                    },
                },
            },
        },
    });
    const result = _.clone(
        testenv.data.tokens[superToken.address].accounts[subscriber].ida
            .subscriptions[`${publisher}@${indexId}`]
    );
    // calculate pendingDistribution
    const idata = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    if (!result.approved) {
        result.pendingDistribution = toBN(result.units).mul(
            toBN(idata.indexValue).sub(toBN(result._syncedIndexValue))
        );
    } else {
        result.pendingDistribution = toBN(0);
    }
    return result;
}

export async function shouldCreateIndex({
    testenv,
    superToken,
    publisherName,
    indexId,
}: IDABaseParams & {publisherName: string}) {
    console.log("======== shouldCreateIndex begins ========");
    const publisher = testenv.getAddress(publisherName);

    const tx = await web3tx(
        testenv.sf.ida.createIndex,
        `${publisherName} create index ${indexId}`
    )({
        superToken: superToken.address,
        publisher,
        indexId,
    });

    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            indexValue: toBN(0),
            totalUnitsApproved: toBN(0),
            totalUnitsPending: toBN(0),
        },
    });
    const idataExpected = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const idataActual = await testenv.contracts.ida.getIndex(
        superToken.address,
        publisher,
        indexId
    );
    _assertEqualIndexData(idataActual, idataExpected);

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexCreated",
        {
            token: superToken.address,
            publisher: publisher,
            indexId: indexId.toString(),
            userData: null,
        }
    );

    console.log("======== shouldCreateIndex ends ========");
}

export async function shouldDistribute({
    testenv,
    superToken,
    publisherName,
    indexId,
    indexValue,
    amount,
    fn,
}: IDABaseParams & {
    publisherName: string;
    indexValue?: BigNumber;
    amount?: BigNumber;
    fn?: () => Promise<any>;
}) {
    console.log("======== shouldDistribute begins ========");
    const publisher = testenv.getAddress(publisherName);

    const idataBefore = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const subscribers = getSubscribers({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const subscriberAddresses = Object.keys(subscribers);
    const totalUnitsZero = idataBefore.totalUnitsApproved
        .add(idataBefore.totalUnitsPending)
        .eq(toBN(0));
    let tx: any;
    if (totalUnitsZero) {
        indexValue = idataBefore.indexValue;
        tx = await web3tx(
            testenv.sf.ida.distribute,
            `${publisherName} does not distribute tokens to index @${indexId} with amount ${amount} because units = 0`
        )({
            superToken: superToken.address,
            publisher,
            indexId,
            amount,
        });
    } else if (fn) {
        indexValue = (
            await testenv.contracts.ida.calculateDistribution(
                superToken.address,
                publisher,
                indexId,
                toBN(amount)
            )
        ).newIndexValue;
        tx = await fn();
    } else if (indexValue) {
        tx = await web3tx(
            testenv.sf.ida.updateIndex,
            `${publisherName} distributes tokens to index @${indexId} with indexValue ${indexValue}`
        )({
            superToken: superToken.address,
            publisher,
            indexId,
            indexValue,
        });
    } else if (amount) {
        indexValue = (
            await testenv.contracts.ida.calculateDistribution(
                superToken.address,
                publisher,
                indexId,
                amount
            )
        ).newIndexValue;
        tx = await web3tx(
            testenv.sf.ida.distribute,
            `${publisherName} distributes tokens to index @${indexId} with amount ${amount}`
        )({
            superToken: superToken.address,
            publisher,
            indexId,
            amount,
        });
    }

    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            indexValue: toBN(indexValue),
        } as any,
    });
    const idataExpected = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const idataActual = await testenv.contracts.ida.getIndex(
        superToken.address,
        publisher,
        indexId
    );
    _assertEqualIndexData(
        {
            exist: idataActual.exist,
            indexValue: idataActual.indexValue,
            totalUnitsApproved: idataActual.totalUnitsApproved,
            totalUnitsPending: idataActual.totalUnitsPending,
        },
        idataExpected
    );

    // if we are distributing where the totalUnits is zero
    // _updateIndex is not called and there will be no emitted event
    if (!totalUnitsZero) {
        // expect events
        await expectEvent.inTransaction(
            tx.tx,
            testenv.sf.contracts.IInstantDistributionAgreementV1,
            "IndexUpdated",
            {
                token: superToken.address,
                publisher: publisher,
                indexId: indexId.toString(),
                oldIndexValue: idataBefore.indexValue,
                newIndexValue: indexValue,
                totalUnitsPending: idataExpected.totalUnitsPending,
                totalUnitsApproved: idataExpected.totalUnitsApproved,
                userData: null,
            }
        );
    }

    // expect balances
    await testenv.validateExpectedBalances(() => {
        const indexDelta = toBN(idataActual.indexValue).sub(
            toBN(idataBefore.indexValue)
        );

        // publisher
        testenv.updateAccountExpectedBalanceDelta(
            superToken.address,
            publisher,
            toBN(indexDelta)
                .mul(toBN(idataActual.totalUnitsApproved))
                .mul(toBN(-1))
        );

        // subscribers
        subscriberAddresses.forEach((subscriber) => {
            const sdata = getSubscriptionData({
                testenv,
                superToken,
                publisher,
                indexId,
                subscriber,
            });
            const expectedBalanceDelta = toBN(sdata.units).mul(indexDelta);
            if (subscribers[subscriber].approved) {
                testenv.updateAccountExpectedBalanceDelta(
                    superToken.address,
                    subscriber,
                    expectedBalanceDelta
                );
            }
        });
    });

    // expect subscription data of subscribers
    for (let i = 0; i < subscriberAddresses.length; ++i) {
        const subscriber = subscriberAddresses[i];
        const sdataExpected = getSubscriptionData({
            testenv,
            superToken,
            publisher,
            indexId,
            subscriber,
        });
        const sdataActual = await testenv.contracts.ida.getSubscription(
            superToken.address,
            publisher,
            indexId,
            subscriber
        );
        console.log(
            `${testenv.toAlias(
                subscriber
            )} should have pending distribution ${wad4human(
                sdataExpected.pendingDistribution
            )} (${sdataExpected.pendingDistribution})`
        );
        _assertEqualSubscriptionData(sdataActual, sdataExpected);
    }

    console.log("======== shouldDistribute ends ========");
}

function _beforeSubscriptionUpdate({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
}: IDABaseParams & {publisher: string; subscriber: string}) {
    const idataBefore = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });

    const sdataBefore = getSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    return {idataBefore, sdataBefore};
}

async function _afterSubscriptionUpdate({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
    idataBefore,
    sdataBefore,
}: IDABaseParams & {
    publisher: string;
    subscriber: string;
    idataBefore: IDAIndexData;
    sdataBefore: IDASubscriptionData;
}) {
    const sdataExpected = getSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    const sdataActual = await testenv.contracts.ida.getSubscription(
        superToken.address,
        publisher,
        indexId,
        subscriber
    );
    _assertEqualSubscriptionData(sdataActual, sdataExpected);

    const idataExpected = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const idataActual = await testenv.contracts.ida.getIndex(
        superToken.address,
        publisher,
        indexId
    );
    _assertEqualIndexData(idataActual, idataExpected);

    // expect balances
    await testenv.validateExpectedBalances(() => {
        if (!sdataBefore.approved) {
            // side effect of distributing to the pending subscription
            // for each subscription related operation
            const indexDiff = toBN(idataBefore.indexValue).sub(
                toBN(sdataBefore._syncedIndexValue)
            );
            const distribution = indexDiff.mul(toBN(sdataBefore.units));
            testenv.updateAccountExpectedBalanceDelta(
                superToken.address,
                publisher,
                toBN(0).sub(distribution)
            );
            testenv.updateAccountExpectedBalanceDelta(
                superToken.address,
                subscriber,
                distribution
            );
        }
    });
}

export async function shouldApproveSubscription({
    testenv,
    superToken,
    publisherName,
    indexId,
    subscriberName,
    userData,
}: IDABaseParams & {
    publisherName: string;
    subscriberName: string;
    userData?: string;
}) {
    console.log("======== shouldApproveSubscription begins ========");
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const {idataBefore, sdataBefore} = _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = await web3tx(
        testenv.sf.ida.approveSubscription,
        `${subscriberName} approves subscription to index ${publisherName}@${indexId}`
    )({
        superToken: superToken.address,
        publisher,
        indexId,
        subscriber,
        userData,
    });

    // update subscribers list
    _.merge(
        getSubscribers({
            testenv,
            superToken,
            publisher,
            indexId,
        }),
        {
            [subscriber]: {
                approved: true,
            },
        }
    );
    // update subscription data
    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            exist: true,
            approved: true,
            _syncedIndexValue: idataBefore.indexValue,
        },
    });
    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            totalUnitsApproved: toBN(idataBefore.totalUnitsApproved).add(
                toBN(sdataBefore.units)
            ),
            totalUnitsPending: toBN(idataBefore.totalUnitsPending).sub(
                toBN(sdataBefore.units)
            ),
        },
    });

    await _afterSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        idataBefore,
        sdataBefore,
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexSubscribed",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionApproved",
        {
            token: superToken.address,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            userData: userData || null,
        }
    );

    console.log("======== shouldApproveSubscription ends ========");

    return tx;
}

export async function shouldUpdateSubscription({
    testenv,
    superToken,
    publisherName,
    indexId,
    subscriberName,
    units,
    userData,
    fn,
}: IDABaseParams & {
    publisherName: string;
    subscriberName: string;
    userData?: string;
    units: BigNumber;
    fn?: () => Promise<any>;
}) {
    console.log("======== shouldUpdateSubscription begins ========");
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const {idataBefore, sdataBefore} = _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = !fn
        ? await web3tx(
              testenv.sf.ida.updateSubscription,
              `${publisherName} updates subscription from ${subscriberName} of index @${indexId} with ${units} units`
          )({
              superToken: superToken.address,
              publisher,
              indexId,
              subscriber,
              units,
              userData,
          })
        : await fn();

    // update subscribers list
    _.merge(
        getSubscribers({
            testenv,
            superToken,
            publisher,
            indexId,
        }),
        {
            [subscriber]: {},
        }
    );
    // update subscription data
    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            exist: true,
            units,
            _syncedIndexValue: idataBefore.indexValue,
        },
    });
    // update index data
    const unitsDiff = toBN(units).sub(toBN(sdataBefore.units));
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            ...(sdataBefore.approved
                ? {
                      totalUnitsApproved: toBN(
                          idataBefore.totalUnitsApproved
                      ).add(unitsDiff),
                  }
                : {
                      totalUnitsPending: toBN(
                          idataBefore.totalUnitsPending
                      ).add(unitsDiff),
                  }),
        },
    });

    await _afterSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        idataBefore,
        sdataBefore,
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnitsUpdated",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            units,
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionUnitsUpdated",
        {
            token: superToken.address,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            units,
            userData: userData || null,
        }
    );

    console.log("======== shouldUpdateSubscription ends ========");

    return tx;
}

export async function shouldRevokeSubscription({
    testenv,
    superToken,
    publisherName,
    indexId,
    subscriberName,
    userData,
}: IDABaseParams & {
    publisherName: string;
    subscriberName: string;
    userData?: string;
}) {
    console.log("======== shouldRevokeSubscription begins ========");
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const {idataBefore, sdataBefore} = _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = await web3tx(
        testenv.sf.ida.revokeSubscription,
        `${subscriberName} revoke subscription to index ${publisherName}@${indexId}`
    )({
        superToken: superToken.address,
        publisher,
        indexId,
        subscriber,
        userData,
    });

    // update subscribers list
    delete getSubscribers({
        testenv,
        superToken,
        publisher,
        indexId,
    })[subscriber];
    // update subscription data
    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            exist: true,
            approved: false,
            _syncedIndexValue: idataBefore.indexValue,
        },
    });
    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            totalUnitsApproved: toBN(idataBefore.totalUnitsApproved).sub(
                toBN(sdataBefore.units)
            ),
            totalUnitsPending: toBN(idataBefore.totalUnitsPending).add(
                toBN(sdataBefore.units)
            ),
        },
    });

    await _afterSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        idataBefore,
        sdataBefore,
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnsubscribed",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionRevoked",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );

    console.log("======== shouldRevokeSubscription ends ========");

    return tx;
}

export async function shouldDeleteSubscription({
    testenv,
    superToken,
    publisherName,
    indexId,
    subscriberName,
    senderName,
    userData,
}: IDABaseParams & {
    publisherName: string;
    senderName: string;
    subscriberName: string;
    userData?: string;
}) {
    console.log("======== shouldDeleteSubscription begins ========");
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);

    const {idataBefore, sdataBefore} = _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = await web3tx(
        testenv.sf.ida.deleteSubscription,
        `${senderName} deletes subscription from ${subscriberName} to index ${publisherName}@${indexId}`
    )({
        superToken: superToken.address,
        publisher,
        indexId,
        subscriber,
        sender,
        userData,
    });

    // update subscribers list
    delete getSubscribers({
        testenv,
        superToken,
        publisher,
        indexId,
    })[subscriber];
    // update subscription data
    _deleteSubscription({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            ...(sdataBefore.approved
                ? {
                      totalUnitsApproved: toBN(
                          idataBefore.totalUnitsApproved
                      ).sub(toBN(sdataBefore.units)),
                  }
                : {
                      totalUnitsPending: toBN(
                          idataBefore.totalUnitsPending
                      ).sub(toBN(sdataBefore.units)),
                  }),
        },
    });

    await _afterSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        idataBefore,
        sdataBefore,
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnsubscribed",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionRevoked",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnitsUpdated",
        {
            token: superToken.address,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            units: "0",
            userData: userData || null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionUnitsUpdated",
        {
            token: superToken.address,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            units: "0",
            userData: userData || null,
        }
    );

    console.log("======== shouldDeleteSubscription ends ========");

    return tx;
}

export async function shouldClaimPendingDistribution({
    testenv,
    superToken,
    publisherName,
    indexId,
    subscriberName,
    senderName,
    userData,
}: IDABaseParams & {
    publisherName: string;
    senderName: string;
    subscriberName: string;
    userData?: string;
}) {
    console.log("======== shouldClaimPendingDistribution begins ========");

    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);

    const {idataBefore, sdataBefore} = _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = await web3tx(
        testenv.sf.ida.claim,
        `${subscriberName} claims pending distributions from ${publisherName}@${indexId}`
    )({
        superToken: superToken.address,
        publisher,
        indexId,
        subscriber,
        sender,
        userData,
    });

    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            exist: true,
            _syncedIndexValue: idataBefore.indexValue,
        },
    });

    await _afterSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        idataBefore,
        sdataBefore,
    });

    console.log("======== shouldClaimPendingDistribution ends ========");

    return tx;
}
