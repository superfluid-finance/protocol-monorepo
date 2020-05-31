const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx
} = require("@decentral.ee/web3-test-helpers");

contract("Flow Agreement Behaviour", accounts => {

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

    /*
    it("#1 - Should not work with wrong state size - assert revert message", async () => {

        let noise = await web3.eth.abi.encodeParameter("uint256", 10);
        let emitError = false;
        try {
            await agreement.decodeFlow.call(noise);
        } catch(err) {
            console.log(err);
            emitError = true;
            assert.ok(err.toString().includes("invalid state"), "invalid state");
        }

        if(!emitError) {
            throw ("Call: FlowAgreement.decodeFlow - error not emitted");
        }
    });
    */
});

