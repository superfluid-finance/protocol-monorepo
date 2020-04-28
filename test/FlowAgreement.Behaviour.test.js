const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx
} = require("@decentral.ee/web3-test-helpers");

contract("Flow Agreement", accounts => {

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[2];

    let agreement;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);
    });

    beforeEach(async () => {

        agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from:admin
            });
    });

    it("should not work with wrong state size", async () => {

        let noise = await web3.eth.abi.encodeParameter("uint256", 10);
        let emitError = false;

        try {
            await agreement.updateAccount.call(noise);
        } catch(err) {
            emitError = true;
            assert.ok(err.toString().includes("invalid state"), "invalid state");
        }

        if(!emitError) {
            throw ("Call: FlowAgreement.updateAccount - error not emitted");
        }
    });
});

