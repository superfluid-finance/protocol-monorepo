const { ethers, web3 } = require("hardhat");
const { assert } = require("chai");
const { Framework } = require("@superfluid-finance/sdk-core");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

const daiABI = require("./abis/fDAIABI");
import { SignerWithAddress } from "@nomiclabs/hardhat-ethers/signers";

const traveler = require("ganache-time-traveler");
const TEST_TRAVEL_TIME = 3600 * 2; // 1 hours

const provider = web3;

const errorHandler = (err: any) => {
  if (err) throw err;
};

const ZERO_ADDRESS = "0x" + "0".repeat(40);
let accounts: SignerWithAddress[];
let admin: SignerWithAddress, alice: SignerWithAddress, bob: SignerWithAddress,
    chris: SignerWithAddress, dave: SignerWithAddress, emma: SignerWithAddress,
    frank: SignerWithAddress;
let users = new Map();

let sf: any;
let dai: any;
let daix: any;
let app: any;
const markup = 110000;
var minStep: number;
let superSigner: SignerWithAddress;

before(async function () {
  accounts = (await ethers.getSigners()).slice(0, 7);
  [admin, alice, bob, chris, dave, emma, frank] = accounts;
  users.set(admin, "Admin");
  users.set(alice, "Alice");
  users.set(bob, "Bob");
  users.set(chris, "Chris");
  users.set(dave, "Dave");
  users.set(emma, "Emma");
  users.set(frank, "Frank");
  users.set(ZERO_ADDRESS, "0x0");

  await deployFramework(errorHandler, {
    web3,
    from: admin.address,
  });

  await deployTestToken(errorHandler, [":", "fDAI"], {
    web3,
    from: admin.address,
  });

  await deploySuperToken(errorHandler, [":", "fDAI"], {
    web3,
    from: admin.address,
  });

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
  let daiAddress = daix.underlyingToken.address;

  //get the contract object for the erc20 token
  dai = new ethers.Contract(daiAddress, daiABI, accounts[0]);
});

beforeEach(async function () {
  let App = await ethers.getContractFactory("Auction", accounts[0]);
  app = await App.deploy(
    sf.settings.config.hostAddress,
    sf.settings.config.cfaV1Address,
    daix.address,
    markup.toString() // STEP AMOUNT;
  );

  users.set(app, "App");
  console.log("app.address: ", app.address);

  const daixUpgradeOperation = daix.upgrade({
    amount: ethers.utils.parseEther("10000000")
  });

  for (let i = 0; i < accounts.length; ++i) {
    await dai.connect(accounts[i]).mint(accounts[i].address, ethers.utils.parseEther("10000000"));
    await dai.connect(accounts[i]).approve(daix.address, ethers.utils.parseEther("10000000"));
    await daixUpgradeOperation.exec(accounts[i]);
    checkBalance(accounts[i]);
  }
  const daixTransferOperation = daix.transfer({
    receiver: app.address,
    amount: ethers.utils.parseEther("1")
  });

  await daixTransferOperation.exec(bob);
});

async function checkBalance(who: SignerWithAddress = alice) {
  const daiBal = await daix.balanceOf({ account: who.address, providerOrSigner: accounts[0] });
  console.log(users.get(who), 'daix bal for acct 0: ', daiBal, "who address:", who.address);
}

async function checkBalances(accounts: SignerWithAddress[]) {
  for (let i = 0; i < accounts.length; ++i) {
    await checkBalance(accounts[i]);
  }
}

async function send(who: SignerWithAddress, amount: number) {
  console.log(
    users.get(who) + " sends a stream of " + amount + " DAI to the app"
  );
  minStep = Number((await app.minStep.call()).toString()) / 100000;
  console.log("minStep: ", minStep);
  if (amount < minStep) amount += Math.ceil(minStep);
  try {
    await sendDai(who, app, amount);
  } catch (e) {
    console.log("error oh no: ", e);
  }
}

async function sendDai(from: SignerWithAddress, to: SignerWithAddress, amount: number) {
  console.log(users.get(from) + "\t->\t", users.get(to) + "\tDAI/s:\t", amount);
  const createFlowOperation = sf.cfaV1.createFlow({
    receiver: app.address,
    superToken: daix.address,
    flowRate: Number(amount).toString()
  });

  const txn = await createFlowOperation.exec(from);
  const receipt = await txn.wait();
};

async function update(who: SignerWithAddress, amount: number) {
  console.log(
    users.get(who) + " updates their stream to " + amount + " DAI to the app"
  );
  minStep = Number((await app.minStep.call()).toString()) / 100000;
  console.log("minStep: ", minStep);
  if (amount < minStep) amount += Math.ceil(minStep);
  try {
    await updateDai(who, app, amount);
  } catch (e) {
    console.log("error oh no: ", e);
  }
}

async function updateDai(from: SignerWithAddress, to: SignerWithAddress, amount: number) {
  console.log(users.get(from) + "\t->\t", users.get(to) + "\tDAI/s:\t", amount);
  const updateOperation = sf.cfaV1.updateFlow({
    receiver: app.address,
    superToken: daix.address,
    flowRate: Number(amount).toString()
  });

  const txn = await updateOperation.exec(from);
  const receipt = await txn.wait();
};

async function close(who: SignerWithAddress) {
  console.log(users.get(who) + " closes stream");
  try {
    await closeStream(who, app);
  } catch (e) {
    console.log("Oh no! Error closing: ", e);
  }
}

async function closeStream(from: SignerWithAddress, to: SignerWithAddress) {
  const delFlowOperation = sf.cfaV1.deleteFlow({
    sender: from.address,
    receiver: to.address,
    superToken: daix.address,
  });
  const txn = await delFlowOperation.exec(from);
  const receipt = await txn.wait();
}

async function flowExists(who: SignerWithAddress) {
  const value = await sf.cfaV1.getNetFlow({
    superToken: daix.address,
    account: who.address,
    providerOrSigner: superSigner
  });
  console.log("flow of: " + users.get(who) + " exists? " + value);
  return value;
}

describe("Auction", function () {
  it("Case #1 - Alice joins auction, then leaves", async function () {
    minStep = 0;
    checkBalance(alice);
    assert.equal(app.address, await app.winner.call(), "owner isn't equal");

    await send(alice, 15);
    assert.equal(alice.address, await app.winner.call(), "owner isn't equal");

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
    await checkBalance();

    close(alice);

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
    await checkBalance(alice);
    await checkBalance(app);
  });

  it("Case #2 - Alice joins auction, then Bob joins", async function () {
    minStep = 0;
    await send(alice, 15);
    await send(bob, 40);

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

    await close(alice);
    await send(alice, 41);
    await close(alice);

    await send(alice, 15);

    await close(alice);

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);

    await send(alice, 45);

    await close(alice);

    await close(bob);

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
    await checkBalances(accounts);
    await checkBalance(app);
  });

  it("Case #3 - Everyone puts the same bid, then leaves", async () => {
    minStep = 0;

    for (let account of accounts) {
      await send(account, 100);
    }

    for (let account of accounts) {
      await close(account);
    }

    console.log("winner: ", await app.winner.call());
  });

  it("Case #4 - Everyone put bid plus two", async () => {
    minStep = 0;
    await checkBalances(accounts);
    await checkBalance(app);

    let bid_amount: number = 0;
    for (let account of accounts) {
      await send(account, 50 + bid_amount);
      console.log("go forward in time");
      await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
      bid_amount += 2;
    }
    
    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
    await checkBalances(accounts);
    await checkBalance(app);
    
    assert.equal(frank.address, await app.winner.call(), "owner isn't equal");
  });

  it("Case #5 - updating streams", async () => {
    minStep = 0;
    await checkBalances(accounts);
    await checkBalance(app);

    await send(alice, 10);
    await update(alice, 15);
    await send(bob, 10);
    await update(bob, 30);
    await update(bob, 20);
    await update(bob, 17);
    await update(bob, 16);

    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
    await checkBalances(accounts);
    await checkBalance(app);
  });
});

it("Case #6 - Everyone joins, then they randomly upgrade their bid", async () => {
  minStep = 0;
  await checkBalances(accounts);
  await checkBalance(app);

  for (let account of accounts) {
    await send(account, Math.floor(Math.random() * 100 + 1));
    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
  }

  for (var i = 0; i < 30; i++) {
    var rand = Math.floor(Math.random() * accounts.length);
    await update(
      accounts[rand],
      Math.floor(Math.random() * 100 + 1)
    );
    console.log("go forward in time");
    await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
  }

  console.log("go forward in time");
  await traveler.advanceTimeAndBlock(TEST_TRAVEL_TIME);
  await checkBalances(accounts);
  await checkBalance(app);
}).timeout(10000000);
