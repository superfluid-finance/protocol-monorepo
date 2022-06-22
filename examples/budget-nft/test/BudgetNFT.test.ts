
let { toWad } = require("@decentral.ee/web3-helpers");
let { Framework } = require("@superfluid-finance/sdk-core");
let { assert } = require("chai");
let { ethers, web3 } = require("hardhat");
let daiABI = require("./abis/fDAIABI");
import traveler from "ganache-time-traveler";
import { BudgetNFT  } from "../typechain";
const TEST_TRAVEL_TIME = 3600 * 2; // 1 hours


let deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
let deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
let deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

let provider = web3;

let accounts: any[]

let sf: InstanceType<typeof Framework>;;
let dai: InstanceType<typeof daiABI>;
let daix: InstanceType<typeof daiABI>;
let superSigner: InstanceType<typeof sf.createSigner>;
let budgetNFT: InstanceType<typeof BudgetNFT>;

let errorHandler = (err: any) => {
  if (err) throw err;
};

before(async function () {
  //get accounts from hardhat
  accounts = await ethers.getSigners();

  //deploy the framework
  await deployFramework(errorHandler, {
    web3,
    from: accounts[0].address,
  });

  //deploy a fake erc20 token
  let fDAIAddress = await deployTestToken(errorHandler, [":", "fDAI"], {
    web3,
    from: accounts[0].address,
  });

  //deploy a fake erc20 wrapper super token around the fDAI token
  let fDAIxAddress = await deploySuperToken(errorHandler, [":", "fDAI"], {
    web3,
    from: accounts[0].address,
  });

  console.log("fDAIxAddress: ", fDAIxAddress);
  console.log("fDAIAddress: ", fDAIAddress);

  //initialize the superfluid framework...put custom and web3 only bc we are using hardhat locally
  sf = await Framework.create({
    networkName: "custom",
    provider,
    dataMode: "WEB3_ONLY",
    resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
    protocolReleaseVersion: "test",
  });

  superSigner = await sf.createSigner({
    signer: accounts[0],
    provider: provider
  });

  //use the framework to get the super token
  daix = await sf.loadSuperToken("fDAIx");

  //get the contract object for the erc20 token
  let daiAddress = daix.underlyingToken.address;
  dai = new ethers.Contract(daiAddress, daiABI, accounts[0]);

  let App = await ethers.getContractFactory("BudgetNFT", accounts[0]);

  //deploy the contract
  budgetNFT = await App.deploy(
    "BudgetNFT",
    "BNFT",
    sf.settings.config.hostAddress,
    daix.address
  );

  const appInitialBalance = await daix.balanceOf({
    account: budgetNFT.address,
    providerOrSigner: accounts[0]
  });

  console.log("appInitialBalance: ", appInitialBalance); // initial balance of the app is 0

  await dai.mint(
    accounts[0].address, ethers.utils.parseEther("1000")
  );

  await dai.approve(daix.address, ethers.utils.parseEther("1000"));

  const daixUpgradeOperation = daix.upgrade({
    amount: ethers.utils.parseEther("1000")
  });

  await daixUpgradeOperation.exec(accounts[0]);

  const daiBal = await daix.balanceOf({
    account: accounts[0].address,
    providerOrSigner: accounts[0]
  });
  console.log('daix bal for acct 0: ', daiBal);

  // add flow to contract
  const createFlowOperation = await sf.cfaV1.createFlow({
    receiver: budgetNFT.address,
    superToken: daix.address,
    flowRate: toWad(0.01).toString(),
  })

  const txn = await createFlowOperation.exec(accounts[0]);
  const receipt = await txn.wait();

  console.log("go forward in time");
  await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

  const balance = await daix.balanceOf({ account: budgetNFT.address, providerOrSigner: accounts[0] });
  console.log('daix bal after flow: ', balance);
});

beforeEach(async function () {
  let alice = accounts[1];

  await dai.connect(alice).mint(
    alice.address, ethers.utils.parseEther("1000")
  );

  await dai.connect(alice).approve(
    daix.address, ethers.utils.parseEther("1000")
  );

  const daixUpgradeOperation = daix.upgrade({
    amount: ethers.utils.parseEther("1000")
  });

  await daixUpgradeOperation.exec(alice);

  const daiBal = await daix.balanceOf({ account: alice.address, providerOrSigner: accounts[0] });
  console.log('daix bal for acct alice: ', daiBal);
});

async function netFlowRate(user: any) {
  const flow = await sf.cfaV1.getNetFlow({
    superToken: daix.address,
    account: user.address,
    providerOrSigner: superSigner
  });
  return flow;
}

describe("issue NFT", async function () {
  it("Case #1 - NFT is issued to Alice", async () => {
    let alice = accounts[1];

    // key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    const aliceFlow = await netFlowRate(alice)
    const appFlowRate = await netFlowRate(budgetNFT);
    const adminFlowRate = await netFlowRate(accounts[0]);

    console.log("alice flow: ", aliceFlow);
    console.log("app flow: ", appFlowRate);
    console.log("admin flow: ", adminFlowRate);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlow.toString(),
      toWad(0.001).toString(),
      "alice flow is inaccurate"
    );

    //make sure app has right flow rate
    assert.equal(
      Number(appFlowRate),
      (Number(adminFlowRate) * - 1) - Number(aliceFlow),
      "app net flow is incorrect"
    );

    //burn NFT created in this test
    await budgetNFT.burnNFT(0);
  });

  it("Case #2 - NFT is edited", async () => {
    let alice = accounts[1];

    // key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    //key action #2 = NFT flowRate is increased. first param here is tokenId, which is now 1
    await budgetNFT.editNFT(
      1,
      toWad(0.002).toString(),
    );

    const aliceFlowAfterIncrease = await netFlowRate(alice);
    const appFlowRateAfterIncrease = await netFlowRate(budgetNFT);
    const adminFlowRateAfterIncrease = await netFlowRate(accounts[0]);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlowAfterIncrease,
      toWad(0.002).toString(),
      "Alice flow rate is inaccurate"
    );

    //make sure app has right flow rate
    assert.equal(
      Number(appFlowRateAfterIncrease),
      (Number(adminFlowRateAfterIncrease) * - 1) - Number(aliceFlowAfterIncrease),
      "app net flow is incorrect"
    );

    //key action #2 = NFT flowRate is decreased. first param here is tokenId, which is now 1
    await budgetNFT.editNFT(
      1,
      toWad(0.0005).toString(),
    );

    const aliceFlow = await netFlowRate(alice);
    const appFlowRate = await netFlowRate(budgetNFT);
    const adminFlowRate = await netFlowRate(accounts[0]);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlow,
      toWad(0.0005).toString(),
      "Alice flow rate is inaccurate"
    );

    //make sure app has right flow rate
    assert.equal(
      Number(appFlowRate),
      (Number(adminFlowRate) * - 1) - Number(aliceFlow),
      "app net flow is incorrect"
    );

    //burn NFT created in this test
    await budgetNFT.burnNFT(1);
  });
});

describe("burn NFT", async function () {
  it("Case #1 - NFT is issued to Alice, then burned", async () => {
    let alice = accounts[1];

    //key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    const aliceFlow = await netFlowRate(alice);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlow,
      toWad(0.001).toString(),
      "Alice flow rate is inaccurate"
    );

    //key action #2 - NFT is burned, which should turn off flow rate (this token id is number 2)
    await budgetNFT.burnNFT(2);

    const aliceFlowAfterBurned = await netFlowRate(alice);
    console.log("Alice flow rate after ID #2 is burned " + aliceFlowAfterBurned);

    const appFlow = await netFlowRate(budgetNFT);
    const adminFlow = await netFlowRate(accounts[0]);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlowAfterBurned,
      0,
      "Alice flow rate is inaccurate, should be zero"
    );

    //make sure app has right flow rate
    assert.equal(
      Number(appFlow),
      (Number(adminFlow) * - 1),
      "app net flow is incorrect"
    );
  });
})

describe("split and merge NFTs", async function () {
  it("Case #1 - NFT is issued to Alice, then split", async () => {
    let alice = accounts[1];

    //key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    //key action #2 - NFT is split, which should cut flow rate in half from each NFT. this token ID is number 3
    await budgetNFT.connect(alice).splitStream(
      3,
      toWad(0.0005).toString(),
    );

    const aliceFlow = await netFlowRate(alice);

    //As alice is the owner of both the NFTs, flow rate should be the the sum of flowrate in both NFTs
    assert.equal(
      aliceFlow,
      toWad(0.001),
      "Alice flow rate is inaccurate, should be the same at first"
    );

    //key action #3 - new NFT creating in split is burned, leaving only 1/2 of alice flow rate left
    //NFT being burned is Alice's 4th token
    await budgetNFT.burnNFT(4);

    const aliceUpdatedFlow = await netFlowRate(alice);

    assert.equal(
      aliceUpdatedFlow,
      toWad(0.0005).toString(),
      "Alice flow rate is inaccurate, should be 1/2 original"
    );

    const appFlow = await netFlowRate(budgetNFT);
    const adminFlow = await netFlowRate(accounts[0]);

    // make sure app has right flow rate
    assert.equal(
      Number(appFlow) + Number(aliceUpdatedFlow),
      (Number(adminFlow) * - 1),
      "app net flow is incorrect"
    );

    //burn NFT created in this test - #4 is already burned, so need to also burn 3
    await budgetNFT.burnNFT(3);
  });

  it("Case #2 - NFT is issued to Alice, split, then merged again", async () => {
    let alice = accounts[1];

    //key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    //key action #2 - NFT is split, which should cut flow rate in half from each NFT
    await budgetNFT.connect(alice).splitStream(
      5,
      toWad(0.0005).toString(),
    );

    const aliceFlow = await netFlowRate(alice);

    console.log(`Alice Flow Rate is now: ${aliceFlow}`);

    //make sure that alice receives correct flow rate
    assert.equal(
      aliceFlow,
      toWad(0.001),
      "Alice flow rate is inaccurate, should be the same at first"
    );

    //key action #3 - 2 new NFTs are merged, alice should still have 100% of flow rate
    //note: the newly split NFT from previous action in this case is now ID #6. it is also burned by this action
    await budgetNFT.connect(alice).mergeStreams(5, 6);

    const aliceUpdatedFlow = await netFlowRate(alice);
    const adminFlow = await netFlowRate(accounts[0]);
    const appFlow = await netFlowRate(budgetNFT);

    assert.equal(
      aliceUpdatedFlow,
      toWad(0.001),
      "Alice flow rate is inaccurate, should be 100% of original"
    );

    // make sure app has right flow rate
    assert.equal(
      Number(appFlow) + Number(aliceUpdatedFlow),
      (Number(adminFlow) * - 1),
      "app net flow is incorrect"
    );

    //burn NFT created in this test
    await budgetNFT.burnNFT(5);
  });

  it("Case #3 - NFT is issued to Alice, then split and one of the NFTs is transferred to bob", async () => {
    let alice = accounts[1];
    let bob = accounts[2];

    //key action - NFT is issued to alice w flowrate
    await budgetNFT.issueNFT(
      alice.address,
      toWad(0.001).toString(),
    );

    //key action #2 - NFT is split at a ratio of 25/75
    await budgetNFT.connect(alice).splitStream(
      7,
      toWad(0.00075).toString(),
    );

    const aliceFlow = await netFlowRate(alice);
    const bobFlow = await netFlowRate(bob);

    //As alice is the owner of both the NFTs, flow rate should be the the sum of flowrate in both NFTs
    assert.equal(
      aliceFlow,
      toWad(0.001),
      "Alice flow rate is inaccurate, should be the same at first"
    );
    assert.equal(
      bobFlow,
      toWad(0),
      "Bob flow rate is inaccurate, should be zero"
    );

    //key action #3 - NFT created in the split is transferred to bob
    await budgetNFT.connect(alice).transferFrom(alice.address, bob.address, 7);

    const aliceUpdatedFlow = await netFlowRate(alice);
    const bobUpdatedFlow = await netFlowRate(bob);
    assert.equal(
      aliceUpdatedFlow,
      toWad(0.00075).toString(),
      "Alice flow rate is inaccurate, should be 1/2 original"
    );
    assert.equal(
      bobUpdatedFlow,
      toWad(0.00025).toString(),
      "Bob flow rate is inaccurate, should be 1/2 original"
    );

    //burn the original NFT and the one created in the split
    await budgetNFT.burnNFT(7);
    await budgetNFT.burnNFT(8);
  });
})