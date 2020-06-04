const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");

contract("Usecase 1 Stories", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const Miao = accounts[1];
    const Fran = accounts[2];
    const Mike = accounts[3];
    const Nuno = accounts[4];

    let token;
    let superToken;
    let agreement;

    beforeEach(async () => {

        agreement = await web3tx(FlowAgreement.new, "FlowAgreement.new")(
            {
                from:admin
            });

        token = await web3tx(TestToken.new, "TestToken.new")(
            {
                from: admin
            });

        await token.mint(Miao, toWad(10));
        await token.mint(Fran, toWad(10));
        await token.mint(Mike, toWad(10));

        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        console.log("SuperToken Address: ", superToken.address);

        await web3tx(token.approve, "token.approve from Miao to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: Miao
            }
        );

        await web3tx(token.approve, "token.approve from Fran to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: Fran
            }
        );

        await web3tx(token.approve, "token.approve from Mike to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: Mike
            }
        );
    });

    it("#1 - Nuno getting rich (One user is receiving many flows)", async () => {

        await superToken.upgrade(toWad(10), {from: Miao});
        await superToken.upgrade(toWad(10), {from: Fran});
        await superToken.upgrade(toWad(10), {from: Mike});

        const flowRate1 = "16534391534391"; // toWad(10) / 7 / 24 / 3600;
        const flowRate2 = "8267195767195";   // toWad(5)  / 7 / 24 / 3600;

        await web3tx(agreement.createFlow, "Miao -> Nuno new Agreement")(
            superToken.address,
            Nuno,
            flowRate1, {
                from: Miao
            }
        );

        await web3tx(agreement.createFlow, "Fran -> Nuno new Agreement")(
            superToken.address,
            Nuno,
            flowRate2, {
                from: Fran
            }
        );

        await web3tx(agreement.createFlow, "Mike -> Nuno new Agreement")(
            superToken.address,
            Nuno,
            flowRate2, {
                from: Mike
            }
        );

        await traveler.advanceTimeAndBlock(7 * 24 * 3600);

        let NunoBalance = await superToken.balanceOf.call(Nuno);
        let MiaoBalance = await superToken.balanceOf.call(Miao);
        let FranBalance = await superToken.balanceOf.call(Fran);
        let MikeBalance = await superToken.balanceOf.call(Mike);

        console.log("Nuno Balance: ", wad4human(NunoBalance));
        console.log("Miao Balance: ", wad4human(MiaoBalance));
        console.log("Fran Balance: ", wad4human(FranBalance));
        console.log("Mike Balance: ", wad4human(MikeBalance));

        console.log("Nuno balance after one week is ", wad4human(NunoBalance));

        await web3tx(superToken.downgrade, "Nuno is downgrading tokens")(
            NunoBalance, {
                from: Nuno
            }
        );

        let NunoFinalBalance = await token.balanceOf.call(Nuno);
        console.log("Nuno Token balance ", NunoFinalBalance.toString());

        assert.equal(NunoBalance.toString(), NunoFinalBalance.toString(), "Nuno final balance is not the flow balance");
    });
});
