const _ = require("lodash");
const { expectEvent } = require("@openzeppelin/test-helpers");
const { web3tx, toBN } = require("@decentral.ee/web3-helpers");

function _updateIndexData({
    testenv,
    superToken,
    publisher,
    indexId,
    indexData,
}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken]: {
                accounts: {
                    [publisher]: {
                        ida: {
                            indicies: {
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

function getIndexData({ testenv, superToken, publisher, indexId }) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken]: {
                accounts: {
                    [publisher]: {
                        ida: {
                            indicies: {
                                [indexId]: {
                                    data: {
                                        exist: true,
                                        indexValue: "0",
                                        totalUnitsApproved: "0",
                                        totalUnitsPending: "0",
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
        testenv.data.tokens[superToken].accounts[publisher].ida.indicies[
            indexId
        ].data
    );
}

function getSubscribers({ testenv, superToken, publisher, indexId }) {
    return testenv.data.tokens[superToken].accounts[publisher].ida.indicies[
        indexId
    ].subscribers;
}

function _updateSubscriptionData({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
    subscriptionData,
}) {
    _.merge(testenv.data, {
        tokens: {
            [superToken]: {
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

function _deleteSubscription({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
}) {
    delete testenv.data.tokens[superToken].accounts[subscriber].ida
        .subscriptions[`${publisher}@${indexId}`];
}

function getSubscriptionData({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
}) {
    _.defaultsDeep(testenv.data, {
        tokens: {
            [superToken]: {
                accounts: {
                    [subscriber]: {
                        ida: {
                            subscriptions: {
                                [`${publisher}@${indexId}`]: {
                                    exist: true,
                                    approved: false,
                                    units: "0",
                                    pendingDistribution: "0",
                                },
                            },
                        },
                    },
                },
            },
        },
    });
    return _.clone(
        testenv.data.tokens[superToken].accounts[subscriber].ida.subscriptions[
            `${publisher}@${indexId}`
        ]
    );
}

async function shouldCreateIndex({ testenv, publisherName, indexId }) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    let idata;

    const tx = await web3tx(
        testenv.sf.ida.createIndex,
        `${publisherName} create index ${indexId}`
    )({
        superToken,
        sender: publisher, // FIXME fix this in js-sdk
        indexId,
    });

    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            indexValue: "0",
            totalUnitsApproved: "0",
            totalUnitsPending: "0",
        },
    });
    const idataExpected = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const idataActual = await testenv.sf.ida.getIndex({
        superToken,
        publisher,
        indexId,
    });
    assert.deepEqual(idataActual, idataExpected);

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexCreated",
        {
            token: superToken,
            publisher: publisher,
            indexId: indexId.toString(),
            userData: null,
        }
    );

    return idata;
}

async function shouldUpdateIndex({
    testenv,
    publisherName,
    indexId,
    indexValue,
}) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);

    const tx = await web3tx(
        testenv.sf.ida.updateIndex,
        `${publisherName} distributes tokens @${indexId} with value ${indexValue}`
    )({
        superToken,
        sender: publisher, // FIXME
        indexId,
        indexValue,
    });

    // update index data
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            indexValue: indexValue.toString(),
        },
    });
    const idataExpected = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const idataActual = await testenv.sf.ida.getIndex({
        superToken,
        publisher,
        indexId,
    });
    assert.deepEqual(idataActual, idataExpected);

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUpdated",
        {
            token: superToken,
            publisher: publisher,
            indexId: indexId.toString(),
            indexValue,
            totalUnitsPending: idataExpected.totalUnitsPending,
            totalUnitsApproved: idataExpected.totalUnitsApproved,
            userData: null,
        }
    );

    // console.log(
    //     "!!! shouldUpdateSubscription",
    //     JSON.stringify(testenv.data, null, 4)
    // );
    return idataActual;
}

async function shouldApproveSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
}) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const tx = await web3tx(
        testenv.sf.ida.approveSubscription,
        `${subscriberName} approves subscription to ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        sender: subscriber, // FIXME
    });

    // update subscribers list
    getSubscribers({
        testenv,
        superToken,
        publisher,
        indexId,
    })[subscriber] = 1;

    // update subscription data
    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            approved: true,
        },
    });
    const sdataExpected = getSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    const sdataActual = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.deepEqual(sdataActual, sdataExpected);

    // update index data
    const currentIndexData = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            totalUnitsApproved: toBN(currentIndexData.totalUnitsApproved)
                .add(toBN(sdataActual.units))
                .toString(),
            totalUnitsPending: toBN(currentIndexData.totalUnitsPending)
                .sub(toBN(sdataActual.units))
                .toString(),
        },
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexSubscribed",
        {
            token: superToken,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionApproved",
        {
            token: superToken,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            userData: null,
        }
    );

    return sdataActual;
}

async function shouldUpdateSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    units,
}) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const tx = await web3tx(
        testenv.sf.ida.updateSupscription,
        `${publisherName} updates subscription from ${subscriberName} to @${indexId}`
    )({
        superToken,
        sender: publisher, // FIXME
        indexId,
        subscriber,
        units,
    });

    // update subscription data
    const sdataBefore = getSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    _updateSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
        subscriptionData: {
            units: units.toString(),
        },
    });
    const sdataExpected = getSubscriptionData({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    const sdataActual = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.deepEqual(sdataActual, sdataExpected);

    // update index data
    const currentIndexData = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    const unitsDiff = toBN(sdataActual.units).sub(toBN(sdataBefore.units));
    _updateIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
        indexData: {
            ...(sdataActual.approved
                ? {
                      totalUnitsApproved: toBN(
                          currentIndexData.totalUnitsApproved
                      )
                          .add(unitsDiff)
                          .toString(),
                  }
                : {
                      totalUnitsPending: toBN(
                          currentIndexData.totalUnitsPending
                      )
                          .add(unitsDiff)
                          .toString(),
                  }),
        },
    });

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnitsUpdated",
        {
            token: superToken,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            units,
            userData: null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionUnitsUpdated",
        {
            token: superToken,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            units,
            userData: null,
        }
    );

    return sdataActual;
}

async function shouldDeleteSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    senderName,
}) {
    let sdata;
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);

    const tx = await web3tx(
        testenv.sf.ida.deleteSupscription,
        `${senderName} deletes subscription from ${subscriberName} to ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
    });

    // update subscribers
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
    sdata = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.isFalse(sdata.exist);

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUnsubscribed",
        {
            token: superToken,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: null,
        }
    );
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "SubscriptionDeleted",
        {
            token: superToken,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            userData: null,
        }
    );

    return sdata;
}

async function shouldClaimPendingDistribution({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    senderName,
}) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);
    await web3tx(
        testenv.sf.ida.claim,
        `${subscriberName} claims pending distrubtions from ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
    });
}

module.exports = {
    getIndexData,
    getSubscribers,
    shouldCreateIndex,
    shouldUpdateIndex,
    getSubscriptionData,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldDeleteSubscription,
    shouldClaimPendingDistribution,
};
