const { web3tx } = require("@decentral.ee/web3-helpers");

async function shouldCreateIndex({ testenv, publisherName, indexId }) {
    const superToken = testenv.contracts.superToken.address;
    const publisher = testenv.getAddress(publisherName);
    let idata;

    idata = await testenv.sf.ida.getIndex({
        superToken,
        publisher,
        indexId,
    });
    assert.isFalse(idata.exist);

    await web3tx(
        testenv.sf.ida.createIndex,
        `${publisherName} create index ${indexId}`
    )({
        superToken,
        sender: publisher, // TODO fix this in js-sdk
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

    sdata = await testenv.sf.ida.getSubscription({
        superToken,
        publisher,
        indexId,
        subscriber,
    });
    assert.isFalse(sdata.exist);

    await web3tx(
        testenv.sf.ida.approveSupscription,
        `${subscriberName} approves subscription to ${publisherName}@${indexId}`
    )({
        superToken,
        publisher,
        indexId,
        sender: subscriber, // TODO
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

module.exports = {
    shouldCreateIndex,
    shouldApproveSubscription,
};
