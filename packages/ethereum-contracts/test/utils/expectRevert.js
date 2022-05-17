const {chaiModule} = require("./chai-setup");
const expectRevertedWith = async (func, errMsg) => {
    await chaiModule.expect(func).to.be.revertedWith(errMsg);
};
const expectReverted = async (func) => chaiModule.expect(func).to.be.reverted;

module.exports = {
    expectRevertedWith,
    expectReverted,
};
