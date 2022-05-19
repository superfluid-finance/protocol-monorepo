let { ethers, web3 } = require("hardhat");
let { artifacts } = require("hardhat");
let { assert } = require("chai");
let { toWad  } = require("@decentral.ee/web3-helpers");
let daiABI = require("./abis/fDAIABI");

import { SuperToken } from "@superfluid-finance/sdk-core";

let { Framework } = require("@superfluid-finance/sdk-core");
let SuperfluidSDK = require("@superfluid-finance/js-sdk");

let deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
let deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
let deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

const DividendRightsToken = artifacts.require("DividendRightsToken");

import { contracts, DividendRightsToken__factory  } from "../typechain";

// Instances
let sf: InstanceType<typeof Framework>;;
let dividendrightstoken: InstanceType<typeof DividendRightsToken__factory>;;
let dai: InstanceType<typeof daiABI>;;
let daix: InstanceType<typeof daiABI>;;
let supersigner: InstanceType<typeof sf.createSigner>;;


let provider = web3;
let accounts: any[];

let errorHandler = ( err:  any ) => {
    if (err) throw err;
};

before(async function () {
    // get hardhat accounts
    accounts = ethers.getSigners();

    const INIT_BALANCE = toWad(100);
    const MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    accounts = accounts.slice(0, 4);
    const [admin, alice, bob, carol] = accounts;
    
    //deploy the framework
  await deployFramework(errorHandler, {
    web3,
    from: accounts[0].address,
  });
  // deploy a fake erc20 token 
  let fDAIAddress = await deployTestToken(errorHandler, [":", "fDAI"], {
    web3, 
    from: accounts[0].address
 });
 //deploy a fake erc20 wrapper super token around the fDAI token
 let fDAIxAddress = await await sf.loadSuperToken("fDAIx");
 
 console.log("fDAIxAddress: ", fDAIxAddress);
 console.log("fDAIAddress: ", fDAIAddress);

 await sf.initialize();

 if (!dai) {
    const daiAddress = await sf.tokens.fDAI.address;
    dai = await sf.contracts.TestToken.at(daiAddress);
    for (let i = 0; i < accounts.length; ++i) {
        await sf(dai.mint, `Accounts ${i} mints manydai `) (
              accounts[i],
              toWad(10000000),
              { from: accounts[i] }
        );
    }
 };

 daix = sf.token.fDAIX;

 let app = await sf(
    DividendRightsToken.new,
    "DividendRightsToken.new by alice" 
)(
    "Dividend Rights Token",
    "DRT",
    daix.address,
    sf.host.address,
    sf.agreements.ida.address,
    { from: alice }
);

 it("#1 end to end scenario", async () => {
    await dai.approve(daix.address, INIT_BALANCE, { from: alice });
    await daix.upgrade(INIT_BALANCE, { from: alice });

    // setup the app
    await sf(daix.approve, "Alice approve the app")(
        app.address,
        MAX_UINT256,
        { from: alice }
    );

    // alice issue rights to bob then got approved
    await sf(app.issue, "Alice issue 100 rights to bob")(bob, "100", {
        from: alice
    });
    assert.equal((await app.balanceOf.call(bob)).toString(), "100");
    assert.isFalse(await app.isSubscribing.call(bob));
    await sf(
        sf.idaV1.approveSubscription(),
        "Bob approves subscription to the app"
    )(
        sf.agreements.ida.address,
        sf.agreements.ida.contract.methods
            .approveSubscription(daix.address, app.address, 0, "0x")
            .encodeABI(),
        "0x", // user data
        {
            from: bob
        }
    );
    assert.isTrue(await app.isSubscribing.call(bob));

    // alice issue rights to carol after approval
    assert.isFalse(await app.isSubscribing.call(carol));
    await sf(
        sf.idaV1.approveSubscription(),
        "Carol approves subscription to the app"
    )(
        sf.agreements.ida.address,
        sf.agreements.ida.contract.methods
            .approveSubscription(daix.address, app.address, 0, "0x")
            .encodeABI(),
        "0x", // user data
        {
            from: carol
        }
    );
    assert.isTrue(await app.isSubscribing.call(carol));
    await sf(app.issue, "Alice issue 200 rights to carol")(
        carol,
        "200",
        { from: alice }
    );
    assert.equal((await app.balanceOf.call(carol)).toString(), "200");

    // console.log("!!!!",
    //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsApproved.toString(),
    //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsPending.toString(),
    //     (await daix.balanceOf.call(alice)).toString(),
    //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, bob)).units.toString(),
    //     (await daix.balanceOf.call(bob)).toString(),
    //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, carol)).units.toString(),
    //     (await daix.balanceOf.call(carol)).toString());

    // alice distribute 3 tokens
    await sf(
        app.distribute,
        "Alice distribute 3 tokens to everyone"
    )(toWad("3"), { from: alice });
    assert.equal(
        (await daix.balanceOf.call(alice)).toString(),
        toWad("97").toString()
    );
    assert.equal(
        (await daix.balanceOf.call(bob)).toString(),
        toWad("1").toString()
    );
    assert.equal(
        (await daix.balanceOf.call(carol)).toString(),
        toWad("2").toString()
    );

    // carol transfer 100 tokens to bob
    await sf(app.transfer, "Carol transfers 100 rights to bob")(
        bob,
        "100",
        { from: carol }
    );
    assert.equal((await app.balanceOf.call(bob)).toString(), "200");
    assert.equal((await app.balanceOf.call(carol)).toString(), "100");

    // console.log("!!!!",
    //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsApproved.toString(),
    //     (await sf.agreements.ida.getIndex.call(daix.address, app.address, 0)).totalUnitsPending.toString(),
    //     (await daix.balanceOf.call(alice)).toString(),
    //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, bob)).units.toString(),
    //     (await daix.balanceOf.call(bob)).toString(),
    //     (await sf.agreements.ida.getSubscription.call(daix.address, app.address, 0, carol)).units.toString(),
    //     (await daix.balanceOf.call(carol)).toString());

    // alice distribute 3 tokens
    await sf(
        app.distribute,
        "Alice distribute 3 tokens to everyone again"
    )(toWad("3"), { from: alice });
    assert.equal(
        (await daix.balanceOf.call(alice)).toString(),
        toWad("94").toString()
    );
    assert.equal(
        (await daix.balanceOf.call(bob)).toString(),
        toWad("3").toString()
    );
    assert.equal(
        (await daix.balanceOf.call(carol)).toString(),
        toWad("3").toString()
    );
});
});