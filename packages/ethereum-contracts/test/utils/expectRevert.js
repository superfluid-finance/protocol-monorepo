const {chaiModule} = require("./chai-setup");
const expectRevert = async (func, errMsg) => {
    await chaiModule.expect(func).to.be.revertedWith(errMsg);
};

module.exports = {
    expectRevert,
};
