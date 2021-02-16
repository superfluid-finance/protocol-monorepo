const { web3tx } = require("@decentral.ee/web3-helpers");

async function shouldCreateIndex({ testenv, publisherName, indexId }) {
    const publisher = testenv.getAddress(publisherName);
    await web3tx(
        testenv.sf.ida.createIndex,
        `${publisherName} create index ${indexId}`
    )({
        superToken: testenv.contracts.superToken.address,
        sender: publisher, // TODO fix this in js-sdk
        indexId,
    });
    const idata = await testenv.sf.ida.getIndex({
        superToken: testenv.contracts.superToken.address,
        publisher,
        indexId,
    });
    assert.isTrue(idata.exist);
    assert.equal(idata.indexValue, "0");
    assert.equal(idata.totalUnitsApproved, "0");
    assert.equal(idata.totalUnitsPending, "0");
}

module.exports = {
    shouldCreateIndex,
};
