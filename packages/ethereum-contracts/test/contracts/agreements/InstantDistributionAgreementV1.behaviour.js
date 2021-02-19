const _ = require("lodash");
const { expectEvent } = require("@openzeppelin/test-helpers");
const { web3tx, toBN, wad4human } = require("@decentral.ee/web3-helpers");

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

function _assertEqualIndexData(idataActual, idataExpected) {
    assert.deepEqual(idataActual, idataExpected);
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

function _assertEqualSubscriptionData(sdataActual, sdataExpected) {
    const sdataExpectedClean = _.clone(sdataExpected);
    delete sdataExpectedClean._syncedIndexValue;
    assert.deepEqual(sdataActual, sdataExpectedClean);
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
                                    exist: false,
                                    approved: false,
                                    units: "0",
                                    _syncedIndexValue: "0",
                                },
                            },
                        },
                    },
                },
            },
        },
    });
    const result = _.clone(
        testenv.data.tokens[superToken].accounts[subscriber].ida.subscriptions[
            `${publisher}@${indexId}`
        ]
    );
    // calculate pendingDistribution
    const idata = getIndexData({
        testenv,
        superToken,
        publisher,
        indexId,
    });
    if (!result.approved) {
        result.pendingDistribution = toBN(result.units)
            .mul(toBN(idata.indexValue).sub(toBN(result._syncedIndexValue)))
            .toString();
    } else {
        result.pendingDistribution = "0";
    }
    return result;
}

async function shouldCreateIndex({ testenv, publisherName, indexId }) {
    console.log("======== shouldCreateIndex begins ========");
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);

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
    _assertEqualIndexData(idataActual, idataExpected);

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

    console.log("======== shouldCreateIndex ends ========");
}

async function shouldDistribute({
    testenv,
    publisherName,
    indexId,
    indexValue,
    amount,
    fn,
}) {
    console.log("======== shouldDistribute begins ========");
    const superToken = testenv.contracts.superToken.address;
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

    let tx;
    if (fn) {
        indexValue = (
            await testenv.sf.agreements.ida.calculateDistribution(
                superToken,
                publisher,
                indexId,
                amount
            )
        ).newIndexValue.toString();
        tx = await fn();
    } else if (indexValue) {
        tx = await web3tx(
            testenv.sf.ida.updateIndex,
            `${publisherName} distributes tokens to index @${indexId} with indexValue ${indexValue}`
        )({
            superToken,
            sender: publisher, // FIXME
            indexId,
            indexValue,
        });
    } else if (amount) {
        indexValue = (
            await testenv.sf.agreements.ida.calculateDistribution(
                superToken,
                publisher,
                indexId,
                amount
            )
        ).newIndexValue.toString();
        tx = await web3tx(
            testenv.sf.ida.distribute,
            `${publisherName} distributes tokens to index @${indexId} with amount ${amount}`
        )({
            superToken,
            sender: publisher, // FIXME
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
    _assertEqualIndexData(idataActual, idataExpected);

    // expect events
    await expectEvent.inTransaction(
        tx.tx,
        testenv.sf.contracts.IInstantDistributionAgreementV1,
        "IndexUpdated",
        {
            token: superToken,
            publisher: publisher,
            indexId: indexId.toString(),
            oldIndexValue: idataBefore.indexValue,
            newIndexValue: indexValue,
            totalUnitsPending: idataExpected.totalUnitsPending,
            totalUnitsApproved: idataExpected.totalUnitsApproved,
            userData: null,
        }
    );

    // expect balances
    await testenv.validateExpectedBalances(() => {
        const indexDelta = toBN(idataActual.indexValue).sub(
            toBN(idataBefore.indexValue)
        );

        // publisher
        testenv.updateAccountExpectedBalanceDelta(
            superToken,
            publisher,
            toBN(indexDelta)
                .mul(toBN(idataActual.totalUnitsApproved))
                .mul(toBN(-1))
                .toString()
            // TODO deposit delta
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
            const expectedBalanceDelta = toBN(sdata.units)
                .mul(indexDelta)
                .toString();
            if (subscribers[subscriber].approved) {
                testenv.updateAccountExpectedBalanceDelta(
                    superToken,
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
        const sdataActual = await testenv.sf.ida.getSubscription({
            superToken,
            publisher,
            indexId,
            subscriber,
        });
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
}) {
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

    return { idataBefore, sdataBefore };
}

async function _afterSubscriptionUpdate({
    testenv,
    superToken,
    publisher,
    indexId,
    subscriber,
    idataBefore,
    sdataBefore,
}) {
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
    _assertEqualSubscriptionData(sdataActual, sdataExpected);

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
                superToken,
                publisher,
                toBN(0).sub(distribution)
            );
            testenv.updateAccountExpectedBalanceDelta(
                superToken,
                subscriber,
                distribution
            );
        }
    });
}

async function shouldApproveSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    userData,
}) {
    console.log("======== shouldApproveSubscription begins ========");
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const { idataBefore, sdataBefore } = await _beforeSubscriptionUpdate({
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
        superToken,
        publisher,
        indexId,
        sender: subscriber, // FIXME
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
            totalUnitsApproved: toBN(idataBefore.totalUnitsApproved)
                .add(toBN(sdataBefore.units))
                .toString(),
            totalUnitsPending: toBN(idataBefore.totalUnitsPending)
                .sub(toBN(sdataBefore.units))
                .toString(),
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
            token: superToken,
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
            token: superToken,
            subscriber,
            publisher,
            indexId: indexId.toString(),
            userData: userData || null,
        }
    );

    console.log("======== shouldApproveSubscription ends ========");

    return tx;
}

async function shouldUpdateSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    units,
    userData,
    fn,
}) {
    console.log("======== shouldUpdateSubscription begins ========");
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const { idataBefore, sdataBefore } = await _beforeSubscriptionUpdate({
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
              superToken,
              sender: publisher, // FIXME
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
            units: units.toString(),
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
                      totalUnitsApproved: toBN(idataBefore.totalUnitsApproved)
                          .add(unitsDiff)
                          .toString(),
                  }
                : {
                      totalUnitsPending: toBN(idataBefore.totalUnitsPending)
                          .add(unitsDiff)
                          .toString(),
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
            token: superToken,
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
            token: superToken,
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

async function shouldRevokeSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    userData,
}) {
    console.log("======== shouldRevokeSubscription begins ========");
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);

    const { idataBefore, sdataBefore } = await _beforeSubscriptionUpdate({
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
        superToken,
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
            totalUnitsApproved: toBN(idataBefore.totalUnitsApproved)
                .sub(toBN(sdataBefore.units))
                .toString(),
            totalUnitsPending: toBN(idataBefore.totalUnitsPending)
                .add(toBN(sdataBefore.units))
                .toString(),
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
            token: superToken,
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
            token: superToken,
            publisher,
            indexId: indexId.toString(),
            subscriber,
            userData: userData || null,
        }
    );

    console.log("======== shouldRevokeSubscription ends ========");

    return tx;
}

async function shouldDeleteSubscription({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    senderName,
    userData,
}) {
    console.log("======== shouldDeleteSubscription begins ========");
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);

    const { idataBefore, sdataBefore } = await _beforeSubscriptionUpdate({
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
        superToken,
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
                      totalUnitsApproved: toBN(idataBefore.totalUnitsApproved)
                          .sub(toBN(sdataBefore.units))
                          .toString(),
                  }
                : {
                      totalUnitsPending: toBN(idataBefore.totalUnitsPending)
                          .sub(toBN(sdataBefore.units))
                          .toString(),
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
            token: superToken,
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
            token: superToken,
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
            token: superToken,
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
            token: superToken,
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

async function shouldClaimPendingDistribution({
    testenv,
    publisherName,
    indexId,
    subscriberName,
    senderName,
    userData,
}) {
    console.log("======== shouldClaimPendingDistribution begins ========");

    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    const subscriber = testenv.getAddress(subscriberName);
    const sender = testenv.getAddress(senderName);

    const { idataBefore, sdataBefore } = await _beforeSubscriptionUpdate({
        testenv,
        superToken,
        publisher,
        indexId,
        subscriber,
    });

    const tx = await web3tx(
        testenv.sf.ida.claim,
        `${subscriberName} claims pending distrubtions from ${publisherName}@${indexId}`
    )({
        superToken,
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

module.exports = {
    getIndexData,
    getSubscribers,
    shouldCreateIndex,
    shouldDistribute,
    getSubscriptionData,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldRevokeSubscription,
    shouldDeleteSubscription,
    shouldClaimPendingDistribution,
};
