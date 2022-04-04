const {expect} = require("./chai-setup");
export const expectRevert = async (func: any, errMsg: string) => {
    await expect(func).to.be.revertedWith(errMsg);
};
