const {chaiModule} = require("./chai-setup");
const expectRevertedWith = async (func, errMsg) => {
    await chaiModule.expect(func).to.be.revertedWith(errMsg);
};
const expectReverted = async (func) => chaiModule.expect(func).to.be.reverted;

const expectCustomError = async (func, contract, customErrorString) => {
    await expect(func).to.be.revertedWithCustomError(
        contract,
        customErrorString
    );
};

module.exports = {
    expectRevertedWith,
    expectReverted,
    expectCustomError,
};
