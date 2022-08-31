const ethers = require("ethers");

const toBN = (x) => ethers.BigNumber.from(x);

const toWad = (x) => ethers.utils.parseUnits(x.toString(), 18);

const max = (a, b) => (a.gt(b) ? a : b);
const min = (a, b) => (a.gt(b) ? b : a);

module.exports = {
    toBN,
    toWad,
    max,
    min,
};
