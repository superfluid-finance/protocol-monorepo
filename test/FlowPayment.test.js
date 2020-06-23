const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");
const TestGovernance = artifacts.require("TestGovernance");
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
    let governance;
    let superToken;
    let fp;

    before(async () => {

        console.log("admin is %s \nuser1 is %s \nuser2 is %s", admin, user1, user2);

    });

    beforeEach(async () => {

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

        token = await web3tx(TestToken.new, "TestToken.new")(
            {
                from: admin
            });

        governance = await web3tx(TestGovernance.new, "Call: TestGovernance.new")(
            token.address,
            admin,
            1,
            3600, {
                from: admin
            });

        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            governance.address,
            "SuperToken",
            "STK",
            18,
            {
                from: admin
            });
    });

    it("#1 - Basic operations - run connect function", async () => {

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
