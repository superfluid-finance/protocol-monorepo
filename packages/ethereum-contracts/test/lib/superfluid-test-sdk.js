/**
 * Test/deploy harness for legacy Truffle-contract helpers.
 * v1.16.0: decoupled from @superfluid-finance/js-sdk package.json dependency;
 * sources live in the deprecated js-sdk workspace package until hardhat-truffle5 removal.
 */
module.exports = {
    loadContracts: require("../../../js-sdk/src/loadContracts"),
    Framework: require("../../../js-sdk/src/Framework"),
};
