const { Framework } = require("@superfluid-finance/sdk-core");
const { assert, expect } = require("chai");
const { ethers, web3 } = require("hardhat");
const daiABI = require("./abis/fDAIABI");
const MoneyRouterArtifact = require("../artifacts/contracts/MoneyRouter.sol/MoneyRouter.json");
const MoneyRouterABI = MoneyRouterArtifact.abi;

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

const provider = web3;

let accounts;

let sf;
let dai;
let daix;
let owner;
let account1;
let account2;
let moneyRouter;


const errorHandler = (err) => {
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

    //deploy a fake erc20 token for borrow token
    let fDAIAddress = await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0].address,
    });

    //deploy a fake erc20 wrapper super token around the fDAI token
    let fDAIxAddress = await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: accounts[0].address,
    });

    //initialize the superfluid framework...put custom and web3 only bc we are using hardhat locally
    sf = await Framework.create({
        chainId: 31337,
        provider,
        resolverAddress: process.env.RESOLVER_ADDRESS, //this is how you get the resolver address
        protocolReleaseVersion: "test",
    });    

    owner = await sf.createSigner({
        signer: accounts[0],
        provider: provider
    });    

    account1 = await sf.createSigner({
        signer: accounts[1],
        provider: provider
    });

    account2 = await sf.createSigner({
        signer: accounts[2],
        provider: provider
    })

    //use the framework to get the super toen
    daix = await sf.loadSuperToken("fDAIx");
    
    //get the contract object for the erc20 token
    let daiAddress = daix.underlyingToken.address;
    dai = new ethers.Contract(daiAddress, daiABI, accounts[0]);
    
    let MoneyRouter = await ethers.getContractFactory("MoneyRouter", accounts[0]);   
    moneyRouter = await MoneyRouter.deploy(
      sf.settings.config.hostAddress,
      owner.address
    );
    await moneyRouter.deployed();
});

beforeEach(async function () {
    console.log("Topping up account balances...");
        
    await dai.connect(owner).mint(
        owner.address, ethers.utils.parseEther("10000")
    );

    await dai.connect(account1).mint(
        account1.address, ethers.utils.parseEther("10000")
    );

    await dai.connect(account2).mint(
        account2.address, ethers.utils.parseEther("1000")
    );    

    await dai.connect(owner).approve(daix.address, ethers.utils.parseEther("10000"));
    await dai.connect(account1).approve(daix.address, ethers.utils.parseEther("10000"));
    await dai.connect(account2).approve(daix.address, ethers.utils.parseEther("1000"));
    
    const ownerDaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("10000")
    });
    const account1DaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("10000")
    });
    const account2DaixUpgradeOperation = daix.upgrade({
        amount: ethers.utils.parseEther("1000")
    });
    
    await ownerDaixUpgradeOperation.exec(owner);
    await account1DaixUpgradeOperation.exec(account1);
    await account2DaixUpgradeOperation.exec(account2);
});

describe("Money Router", function () {
  it("Access Control #1 - Should deploy properly with the correct owner", async function () {
    expect(await moneyRouter.owner()).to.equal(owner.address);
  });
  it("Access Control #2 - Should allow you to add account to account list", async function () {
    await moneyRouter.connect(owner).whitelistAccount(account1.address);

    expect(await moneyRouter.accountList(account1.address), true);
  });
  it("Access Control #3 - Should allow for removing accounts from whitelist", async function () {
      await moneyRouter.connect(owner).removeAccount(account1.address);

      expect(await moneyRouter.accountList(account1.address), true);
  });
  it("Access Control #4 - Should allow for change in ownership", async function () {
      await moneyRouter.connect(owner).changeOwner(account1.address);

      expect(await moneyRouter.owner(), account1.address);
  })
  it("Contract Receives Funds #1 - lump sum is transferred to contract", async function() {
      //transfer ownership back to real owner...
      await moneyRouter.connect(account1).changeOwner(owner.address);

      let daixApproveOperation = daix.approve({receiver: moneyRouter.address, amount: ethers.utils.parseEther("100")});
      await daixApproveOperation.exec(owner);
      await moneyRouter.connect(owner).sendLumpSumToContract(daix.address, ethers.utils.parseEther("100"));

      let contractDAIxBalance = await daix.balanceOf({account: moneyRouter.address, providerOrSigner: owner});
      expect(contractDAIxBalance, ethers.utils.parseEther("100"));
  });
 it("Contract Receives Funds #2 - a flow is created into the contract", async function() {
    let authorizeContractOperation = sf.cfaV1.updateFlowOperatorPermissions({
         superToken: daix.address, 
         flowOperator: moneyRouter.address,
         permissions: "7", //full control
         flowRateAllowance: "1000000000000000" // ~2500 per month
    });
    await authorizeContractOperation.exec(owner);

    await moneyRouter.createFlowIntoContract(daix.address, "100000000000000") //about 250 daix per month

    let ownerContractFlowRate = await sf.cfaV1.getFlow({
        superToken: daix.address,
        sender: owner.address,
        receiver: moneyRouter.address,
        providerOrSigner: owner
    });

    expect(ownerContractFlowRate, "100000000000000");
 });
 it("Contract recieves funds #3 - a flow into the contract is updated", async function () {
     await moneyRouter.updateFlowIntoContract(daix.address, "200000000000000") // about 250 daix per month

     let ownerContractFlowRate = await sf.cfaV1.getFlow({
        superToken: daix.address,
        sender: owner.address,
        receiver: moneyRouter.address,
        providerOrSigner: owner
    });

    expect(ownerContractFlowRate, "200000000000000");
 });
 it("Contract receives funds #4 - a flow into the contract is deleted", async function () {
     await moneyRouter.deleteFlowIntoContract(daix.address);

     let ownerContractFlowRate = await sf.cfaV1.getFlow({
        superToken: daix.address,
        sender: owner.address,
        receiver: moneyRouter.address,
        providerOrSigner: owner
    });

    expect(ownerContractFlowRate, "0");
 });
 it("Contract sends funds #1 - withdrawing a lump sum from the contract", async function () {
     let contractStartingBalance = await daix.balanceOf({account: moneyRouter.address, providerOrSigner: owner});

     await moneyRouter.withdrawFunds(daix.address, ethers.utils.parseEther("10"));

     let contractFinishingBalance = await daix.balanceOf({account: moneyRouter.address, providerOrSigner: owner});

     expect(Number(contractStartingBalance) - 10, contractFinishingBalance)
 });

 it("Contract sends funds #2 - creating a flow from the contract", async function () {
     await moneyRouter.createFlowFromContract(daix.address, account1.address, "100000000000000"); //about 250 per month
     
     let receiverContractFlowRate = await sf.cfaV1.getFlow({
        superToken: daix.address,
        sender: account1.address,
        receiver: moneyRouter.address,
        providerOrSigner: owner
    });

    expect(receiverContractFlowRate, "100000000000000");
 });
 it("Contract sends funds #3 - updating a flow from the contract", async function () {
    await moneyRouter.updateFlowFromContract(daix.address, account1.address, "200000000000000"); //about 500 per month
    
    let receiverContractFlowRate = await sf.cfaV1.getFlow({
       superToken: daix.address,
       sender: account1.address,
       receiver: moneyRouter.address,
       providerOrSigner: owner
   });

   expect(receiverContractFlowRate, "200000000000000");
});
it("Contract sends funds #3 - deleting a flow from the contract", async function () {
    await moneyRouter.deleteFlowFromContract(daix.address, account1.address); //about 500 per month
    
    let receiverContractFlowRate = await sf.cfaV1.getFlow({
       superToken: daix.address,
       sender: account1.address,
       receiver: moneyRouter.address,
       providerOrSigner: owner
   });

   expect(receiverContractFlowRate, "0");
});
});
