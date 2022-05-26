let { artifacts } = require("hardhat");
let { assert } = require("chai");
let { toWad  } = require("@decentral.ee/web3-helpers");
let daiABI = require("./abis/fDAIABI");

import { SuperToken } from "@superfluid-finance/sdk-core";

let { Framework } = require("@superfluid-finance/sdk-core");
let { ethers, web3 } = require("hardhat");
let { Signer } = require("@nomiclabs/hardhat-ethers");

let deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
let deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
let deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

import { DividendRightsToken } from "../typechain-types/contracts";
let DividendRightsContract;

// Instances
let sf: any;
let dividendRightsToken: DividendRightsToken;
// let dai: InstanceType<typeof daiABI>;;
let dai: any;
let daix: any;
// let daix: InstanceType<typeof daiABI>;;
let supersigner: InstanceType<typeof sf.createSigner>;;

let provider = web3;
let accounts: any[];
let INIT_BALANCE: number;
let MAX_UINT256: string;
let admin: typeof Signer;
let alice: typeof Signer;
let bob: typeof Signer;
let carol: typeof Signer;
let app: typeof ethers.Contract;

let errorHandler = ( err:  any ) => {
    if (err) throw err;
};

before(async function () {
    // get hardhat accounts
    accounts = await ethers.getSigners();
    
    INIT_BALANCE = toWad(100);
    MAX_UINT256 = "115792089237316195423570985008687907853269984665640564039457584007913129639935";
    accounts = accounts.slice(0, 4);
    
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
 let fDAIxAddress = await deploySuperToken(errorHandler, [":", "fDAI"], {
    web3,
    from: accounts[0].address,
});

 sf = await Framework.create({
    networkName: "custom",
    provider,
    dataMode: "WEB3_ONLY",
    resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
    protocolReleaseVersion: "test"
 }); 

 admin = await sf.createSigner({
    signer: accounts[0],
    provider: provider
});    

alice = await sf.createSigner({
    signer: accounts[1],
    provider: provider
});

bob = await sf.createSigner({
    signer: accounts[2],
    provider: provider
})

carol = await sf.createSigner({
    signer: accounts[3],
    provider: provider
});

 
  //deploy a fake erc20 wrapper super token around the fDAI token
  daix = await await sf.loadSuperToken("fDAIx");

  console.log("fDAIxAddress: ", daix.address);
  console.log("fDAIAddress: ", fDAIAddress);

  let daiAddress = await daix.underlyingToken.address;
  dai = new ethers.Contract(daiAddress, daiABI, admin);

 await dai.connect(admin).mint(admin.address, ethers.utils.parseEther("100"));
 await dai.connect(alice).mint(alice.address, ethers.utils.parseEther("100"));

 DividendRightsContract = await ethers.getContractFactory("DividendRightsToken", alice);
 dividendRightsToken = await DividendRightsContract.deploy(
     "Dividend Rights Token",
     "DRT",
     daix.address,
     sf.settings.config.hostAddress
 );
 await dividendRightsToken.deployed();
 
});
describe("first test - end to end scenario", async () => {

 it("#1 end to end scenario", async function () {
    
    await dai.connect(alice).approve(daix.address, INIT_BALANCE.toString());

    let aliceUpgradeOp = daix.upgrade({amount: INIT_BALANCE.toString()});
    await aliceUpgradeOp.exec(alice).then(console.log);
    
    // setup the app
    let aliceApproveOp = daix.approve({receiver: dividendRightsToken.address, amount: INIT_BALANCE.toString()});
    await aliceApproveOp.exec(alice).then(console.log)

    // alice issue rights to bob then got approved
    console.log("Alice issues 100 rights tokens to bob");
    await dividendRightsToken.connect(alice).issue(bob.address, "100").then(console.log);

    assert.equal((await dividendRightsToken.connect(alice).balanceOf(bob.address)).toString(), "100");

    assert.isFalse(await dividendRightsToken.connect(alice).isSubscribing(bob.address));

    let bobApproveSubscriptionOperation = sf.idaV1.approveSubscription({superToken: daix.address, publisher: dividendRightsToken.address, indexId: "0"});
    await bobApproveSubscriptionOperation.exec(bob);

    assert.isTrue(await dividendRightsToken.connect(alice).isSubscribing(bob.address));

    // alice issue rights to carol after approval
    assert.isFalse(await dividendRightsToken.connect(alice).isSubscribing(carol.address));

    let carolApproveSubscriptionOperation = sf.idaV1.approveSubscription({superToken: daix.address, publisher: dividendRightsToken.address, indexId: "0"});
    await carolApproveSubscriptionOperation.exec(carol)
   
    assert.isTrue(await dividendRightsToken.connect(alice).isSubscribing(carol.address));

    console.log("Alice issues 200 rights tokens to Carol");
    
    await dividendRightsToken.connect(alice).issue(carol.address, "200");

    assert.equal((await dividendRightsToken.connect(carol).balanceOf(carol.address)).toString(), "200");

    // alice distribute 3 tokens
    await dividendRightsToken.connect(alice).distribute(ethers.utils.parseEther("3"));

    let owner = await dividendRightsToken.owner();
    console.log("owner of the contract is: ", owner);
    console.log("alice address: ", alice.address);
    let contractDAXbal = await daix.balanceOf({account: dividendRightsToken.address, providerOrSigner: alice});
    console.log("contract balance of daix is: ", contractDAXbal);
    
    let balAl = await dividendRightsToken.connect(alice).balanceOf(alice.address);
    let balDaixAl = await daix.balanceOf({account: alice.address, providerOrSigner: alice});
    let daixAllowance = await daix.allowance({owner: alice.address, spender: dividendRightsToken.address, providerOrSigner: alice});
    console.log("d rights token allowance for alice: ", daixAllowance);
    
    console.log("daix bal of alice: ", balDaixAl);
    console.log(balAl.toString());

    let subscriptionDetails = await sf.idaV1.getSubscription({
            superToken: daix.address,
            publisher: dividendRightsToken.address,
            indexId: "0",
            subscriber: carol.address,
            providerOrSigner: alice
          });
    console.log("subscription details for carol: ", subscriptionDetails);
    
    
    assert.equal(
        (await daix.balanceOf({account: bob.address, providerOrSigner: bob})).toString(),
        ethers.utils.parseEther("1").toString()
    );
    assert.equal(
        (await daix.balanceOf({account: carol.address, providerOrSigner: carol})).toString(),
        ethers.utils.parseEther("2").toString()
    );

    assert.equal(
        (await daix.balanceOf({account: alice.address, providerOrSigner: alice})).toString(),
        ethers.utils.parseEther("97").toString()
    );

    console.log("Carol transfers 100 rights tokens to bob");
    await dividendRightsToken.connect(carol).transfer(bob.address, "100");

    assert.equal((await dividendRightsToken.connect(alice).balanceOf(bob.address)).toString(), "200");
    assert.equal((await dividendRightsToken.connect(alice).balanceOf(carol.address)).toString(), "100");

    // alice distribute 3 tokens
    console.log("Alice distributes 3 more tokens to the index");
    await dividendRightsToken.connect(alice).distribute(ethers.utils.parseEther("3"));

    assert.equal(
        (await daix.balanceOf({account: alice.address, providerOrSigner: alice})).toString(),
        ethers.utils.parseEther("94").toString()
    );
    assert.equal(
        (await daix.balanceOf({account: bob.address, providerOrSigner: admin})).toString(),
        ethers.utils.parseEther("3").toString()
    );
    assert.equal(
        (await daix.balanceOf({account: carol.address, providerOrSigner: admin})).toString(),
        ethers.utils.parseEther("3").toString()
    );
});
});