const {expect} = require("chai");
const expectRevertedWith = async (func, errMsg) => {
    await expect(func).to.be.revertedWith(errMsg);
};
const expectReverted = async (func) => expect(func).to.be.reverted;

const expectCustomError = async (func, contract, customErrorString, args) => {
    args
        ? await expect(func)
              .to.be.revertedWithCustomError(contract, customErrorString)
              .withArgs(args)
        : await expect(func).to.be.revertedWithCustomError(
              contract,
              customErrorString
          );
};

module.exports = {
    expectRevertedWith,
    expectReverted,
    expectCustomError,
};
