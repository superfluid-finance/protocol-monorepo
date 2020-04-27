const SuperToken = artifacts.require("SuperToken");
const ERC20Mintable = artifacts.require("ERC20Mintable");
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
    const user2 = accounts[2];

    let token;
    let superToken;
    let fp;

    before(async () => {
        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);

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
        token = await web3tx(ERC20Mintable.new, "ERC20Mintable.new")(
            {
                from: admin
            });
        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });
    });

    it("basic operations", async () => {
        await web3tx(fp.connect, "Call: FlowPayment.connect - user1 -> user2 9.99/mo")(
            superToken.address,
            user1,
            user2,
            toWad(9.99),
            {
                from: user1
            }
        );
    });
});
