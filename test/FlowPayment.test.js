const SuperToken = artifacts.require("SuperToken");
const FlowPayment = artifacts.require("FlowPayment");
const FlowAgreement = artifacts.require("FlowAgreement");
// const {
//     expectRevert
// } = require("@openzeppelin/test-helpers");
const {
    web3tx,
    //wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");


contract("FlowPayment", accounts => {

    const admin = accounts[0];
    const user1 = accounts[1];
    const user2 = accounts[1];

    let token;
    let fp;    

    before(async () => {
        console.log("admin is", admin);
        console.log("user1 is", user1);
        console.log("user2 is", user2);
        const fa = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from: admin
            }
        );
        fp = await web3tx(FlowPayment.new, "FlowPayment.new")(
            fa.address,
            {
                from: admin
            }
        );
    });
    
    beforeEach(async () => {
        token = await web3tx(SuperToken.new, "SuperToken.new")(
            {
                from: admin
            });
    });

    it("basic operations", async () => {
        await web3tx(fp.connect, "fp.connect user1 -> user2 9.99/mo")(
            token.address,
            user1,
            user2,
            2 /* FLOW_PER_MONTH */,
            toWad(9.99),
            {
                from: user1
            }
        );
    });
});