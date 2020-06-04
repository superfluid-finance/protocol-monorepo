const SuperToken = artifacts.require("SuperToken");
const TestToken = artifacts.require("TestToken");
const FlowAgreement = artifacts.require("FlowAgreement");

const {
    web3tx,
    wad4human,
    toWad
} = require("@decentral.ee/web3-test-helpers");

const traveler = require("ganache-time-traveler");
const INI_BALANCE = toWad(1000);

contract("Flow Agreement Stories", accounts => {

    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";

    const admin = accounts[0];
    const user1 = accounts[1];
    const Netflic = accounts[2];
    const Spotifai = accounts[3];
    const Zoomer = accounts[4];

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

        await token.mint(user1, toWad(1000));

        superToken = await web3tx(SuperToken.new, "SuperToken.new")(
            token.address,
            "SuperToken",
            "STK",
            {
                from: admin
            });

        await web3tx(token.approve, "token.approve from user1 to SuperToken")(
            superToken.address,
            MAX_UINT256, {
                from: user1
            }
        );
    });

    /*
    * A user start paying to multiple companies, Netflic, Spotifai, etc...
    * Netflic cryptoprice - 9.00 DAI / Month
    * Spotifai cryptoprice - 11.85 DAI / Month
    * Zoomer - 5 DAI / week
    */
    it("#1 - One user paying Many services", async () => {

        console.log("SuperToken Address: ", superToken.address);

        await superToken.upgrade(INI_BALANCE, {from : user1});

        const NetflicPricePerSecond = toWad(9 / 30 / 24 /3600);
        const SpotifaiPricePerSecond = toWad(11.85 / 30 / 24 / 3600);
        const ZoomerPricePerSecond = toWad(9 / 7 / 24 / 3600);

        console.log("Netfic Price: ", NetflicPricePerSecond.toString());
        console.log("Spotifai Price: ", SpotifaiPricePerSecond.toString());
        console.log("Zoomer Price: ", ZoomerPricePerSecond.toString());

        await web3tx(agreement.createFlow, "User1 -> Netflic new Agreement")(
            superToken.address,
            Netflic,
            NetflicPricePerSecond, {
                from: user1
            }
        );

        await web3tx(agreement.createFlow, "User1 -> Spotifai new Agreement")(
            superToken.address,
            Spotifai,
            SpotifaiPricePerSecond, {
                from: user1
            }
        );

        await web3tx(agreement.createFlow, "User1 -> Zoomer new Agreement")(
            superToken.address,
            Zoomer,
            ZoomerPricePerSecond, {
                from: user1
            }
        );

        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        let userBalance = await superToken.balanceOf.call(user1);
        let netBalance = await superToken.balanceOf.call(Netflic);
        let sptBalance = await superToken.balanceOf.call(Spotifai);
        let zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));

        // User receives the bill and think he is paying to much for Zoomer
        await web3tx(agreement.deleteFlow, "User1  -> Zoomer Cancel Subscription")(
            superToken.address,
            user1,
            Zoomer, {
                from: user1
            }
        );

        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        userBalance = await superToken.balanceOf.call(user1);
        netBalance = await superToken.balanceOf.call(Netflic);
        sptBalance = await superToken.balanceOf.call(Spotifai);
        zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));

        //User upgrade netflic to family pack - more 5 DAI per month
        let NetflicFamilyAddOn = toWad(5 / 30 / 24 /3600);
        await web3tx(agreement.updateFlow, "User1 -> Netfic Upgrade Subscription")(
            superToken.address,
            Netflic,
            NetflicFamilyAddOn, {
                from: user1
            }
        );

        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        userBalance = await superToken.balanceOf.call(user1);
        netBalance = await superToken.balanceOf.call(Netflic);
        sptBalance = await superToken.balanceOf.call(Spotifai);
        zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));

        //User dicover that he can listen to music on youfube for free
        await web3tx(agreement.deleteFlow, "User1  -> Spotifai Cancel Subscription")(
            superToken.address,
            user1,
            Spotifai, {
                from: user1
            }
        );

        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        userBalance = await superToken.balanceOf.call(user1);
        netBalance = await superToken.balanceOf.call(Netflic);
        sptBalance = await superToken.balanceOf.call(Spotifai);
        zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));

        //User downgrade the Netflic family pack
        await web3tx(agreement.updateFlow, "User1 -> Netfic Upgrade Subscription")(
            superToken.address,
            Netflic,
            (-1 * NetflicFamilyAddOn), {
                from: user1
            }
        );


        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        userBalance = await superToken.balanceOf.call(user1);
        netBalance = await superToken.balanceOf.call(Netflic);
        sptBalance = await superToken.balanceOf.call(Spotifai);
        zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));


        await traveler.advanceTimeAndBlock(3600 * 24 * 30);

        userBalance = await superToken.balanceOf.call(user1);
        netBalance = await superToken.balanceOf.call(Netflic);
        sptBalance = await superToken.balanceOf.call(Spotifai);
        zomBalance = await superToken.balanceOf.call(Zoomer);

        console.log("user: ", wad4human(userBalance));
        console.log("netflic: ", wad4human(netBalance));
        console.log("spotifai: ", wad4human(sptBalance));
        console.log("zoomer: ", wad4human(zomBalance));

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from user1") (
            userBalance, {
                from: user1
            });

        await web3tx(superToken.downgrade, "Call: SuperToken.downgrade - from Netflic") (
            netBalance, {
                from: Netflic
            });
    });
});
