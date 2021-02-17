const { web3tx } = require("@decentral.ee/web3-helpers");

async function shouldCreateIndex({ testenv, publisherName, indexId }) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    let idata;

    await web3tx(
        testenv.sf.ida.createIndex,
        `${publisherName} create index ${indexId}`
    )({
        superToken,
        sender: publisher, // FIXME fix this in js-sdk
        indexId,
    });

    idata = await testenv.sf.ida.getIndex({
        superToken,
        publisher,
        indexId,
    });
    assert.isTrue(idata.exist);
    assert.equal(idata.indexValue, "0");
    assert.equal(idata.totalUnitsApproved, "0");
    assert.equal(idata.totalUnitsPending, "0");

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

    let idata;

    await web3tx(
        testenv.sf.ida.updateIndex,
        `${publisherName} distributes tokens @${indexId} with value ${indexValue}`
    )({
        superToken,
        sender: publisher, // FIXME
        indexId,
        indexValue,
    });

    idata = await testenv.sf.ida.getIndex({
        superToken,
        publisher,
        indexId,
    });

    assert.equal(idata.indexValue.toString(), indexValue);

    return idata;
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

    let sdata;

    await web3tx(
        testenv.sf.ida.approveSupscription,
        `${subscriberName} approves subscription to ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        sender: subscriber, // FIXME
    });

    sdata = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.isTrue(sdata.exist);
    assert.isTrue(sdata.approved);

    return sdata;
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
    let sdata;

    await web3tx(
        testenv.sf.ida.updateSupscription,
        `${publisherName} updates subscription from ${subscriberName} to @${indexId}`
    )({
        superToken,
        sender: publisher, // FIXME
        indexId,
        subscriber,
        units,
    });

    sdata = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.isTrue(sdata.exist);
    assert.equal(sdata.units.toString(), units.toString());

    return sdata;
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
    await web3tx(
        testenv.sf.ida.deleteSupscription,
        `${senderName} deletes subscription from ${subscriberName} to ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        subscriber,
        sender,
    });

    sdata = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.isFalse(sdata.exist);

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
    shouldCreateIndex,
    shouldUpdateIndex,
    shouldApproveSubscription,
    shouldUpdateSubscription,
    shouldDeleteSubscription,
    shouldClaimPendingDistribution,
};
