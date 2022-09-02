const ethers = require("ethers");

const toBN = (x) => ethers.BigNumber.from(x);

const toWad = (x) => ethers.utils.parseUnits(x.toString(), 18);

const max = (a, b) => (a.gt(b) ? a : b);
const min = (a, b) => (a.gt(b) ? b : a);

const keccak256 = (x) => ethers.utils.keccak256(ethers.utils.toUtf8Bytes(x));

const abiCoder = new ethers.utils.AbiCoder();

module.exports = {
    abiCoder,
    keccak256,
    toBN,
    toWad,
    max,
    min,
};
