const { assert, expect } = require("chai");

const { Framework } = require("@superfluid-finance/sdk-core");
const TestTokenABI =  require("@superfluid-finance/ethereum-contracts/build/contracts/TestToken.json");

const { ethers, web3 } = require("hardhat");

const deployFramework = require("@superfluid-finance/ethereum-contracts/scripts/deploy-framework");
const deployTestToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-test-token");
const deploySuperToken = require("@superfluid-finance/ethereum-contracts/scripts/deploy-super-token");

// Instances
let sf;                          // Superfluid framework API object
let spreader;                    // spreader contract object
let dai;                         // underlying token of daix
let daix;                        // will act as `spreaderToken` - is a super token wrapper of dai

// Test Accounts
let admin;      
let alice;      
let bob;

// Constants
let expecationDiffLimit = 10;    // sometimes the IDA distributes a little less wei than expected. Accounting for potential discrepency with 10 wei margin


const errorHandler = (err) => {
  if (err) throw err;
}

before(async function () {

    // get hardhat accounts
    [admin, alice, bob] = await ethers.getSigners();


    //// GETTING SUPERFLUID FRAMEWORK SET UP

    // deploy the framework locally
    await deployFramework(errorHandler, {
        web3: web3,
        from: admin.address,
        // newTestResolver:true
    });

    // initialize framework
    sf = await Framework.create({
        networkName: "custom",
        provider: web3,
        dataMode: "WEB3_ONLY",
        resolverAddress: process.env.RESOLVER_ADDRESS, // (empty)
        protocolReleaseVersion: "test",
    });


    //// DEPLOYING DAI and DAI wrapper super token (which will be our `spreaderToken`)

    // deploy a fake erc20 token
    await deployTestToken(errorHandler, [":", "fDAI"], {
        web3,
        from: admin.address,
    });

    // deploy a fake erc20 wrapper super token around the DAI token
    await deploySuperToken(errorHandler, [":", "fDAI"], {
        web3,
        from: admin.address,
    });

    // deploy a fake erc20 wrapper super token around the DAI token
    daix = await sf.loadSuperToken("fDAIx");

    dai = new ethers.Contract(
        daix.underlyingToken.address,
        TestTokenABI.abi,
        admin
    );


    //// SETTING UP NON-ADMIN ACCOUNTS WITH DAIx

    // minting test DAI
    await dai.connect(admin).mint(admin.address, ethers.utils.parseEther("10000"));
    await dai.connect(alice).mint(alice.address, ethers.utils.parseEther("10000"));
    await dai.connect(bob).mint(bob.address, ethers.utils.parseEther("10000"));

    // approving DAIx to spend DAI (Super Token object is not an ethers contract object and has different operation syntax)
    await dai.connect(admin).approve(daix.address, ethers.constants.MaxInt256);
    await dai.connect(alice).approve(daix.address, ethers.constants.MaxInt256);
    await dai.connect(bob).approve(daix.address, ethers.constants.MaxInt256);

    // Upgrading all DAI to DAIx
    const daiXUpgradeOperation = daix.upgrade({
      amount: ethers.utils.parseEther("10000").toString(),
    })
    await daiXUpgradeOperation.exec(admin);
    await daiXUpgradeOperation.exec(alice);
    await daiXUpgradeOperation.exec(bob);

    
    //// INITIALIZING SPREADER CONTRACT

    const spreaderContractFactory = await ethers.getContractFactory(
        "TokenSpreader",
        admin
    );

    spreader = await spreaderContractFactory.deploy(
        sf.settings.config.hostAddress, 
        daix.address, // Setting DAIx as spreader token
    );


    //// SUBSCRIBING TO SPREADER CONTRACT'S IDA INDEX

    // subscribe to distribution (doesn't matter if this happens before or after distribution execution)
    const approveSubscriptionOperation = await sf.idaV1.approveSubscription({
      indexId: "0",
      superToken: daix.address,
      publisher: spreader.address
    })
    await approveSubscriptionOperation.exec(alice);
    await approveSubscriptionOperation.exec(bob);

    console.log("Set Up Complete! - TokenSpreader Contract Address:", spreader.address);
});

describe("TokenSpreader Test Sequence", async () => {

  it("Distribution with [ no units outstanding ] and [ no spreaderTokens held ]", async function () {
    
    // distribution SHOULD REVERT since no units are outstanding
    await expect( spreader.connect(alice).distribute() ).to.be.reverted;

  })

  it("Distribution with [ 1 unit issued ] but [ 0 spreaderTokens held ] - gainShare", async function () {
    
    //// ACTIONS

    // Alice claims distribution unit
    await spreader.connect(alice).gainShare(alice.address);


    //// EXPECTATIONS

    // expect alice to have 1 distribution unit
    let aliceSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: alice.address, 
      providerOrSigner: alice 
    });

    await expect(
      aliceSubscription.units
    ).to.equal(
      "1"
    );

    // distribution SHOULD NOT REVERT if there are outstanding units issued
    await expect( spreader.connect(alice).distribute() ).to.be.not.reverted;

  })

  it("Distribution with [ 2 units issued to different accounts ] but [ 0 spreaderTokens ] - gainShare", async function () {

    //// ACTIONS

    // Bob claims distribution unit
    await spreader.connect(bob).gainShare(bob.address);


    //// EXPECTATIONS

    // expect alice to have 1 distribution unit
    let aliceSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: alice.address, 
      providerOrSigner: alice 
    });
    
    await expect(
      aliceSubscription.units
    ).to.equal(
      "1"
    );

    // expect bob to have 1 distribution unit
    let bobSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: bob.address, 
      providerOrSigner: bob 
    });

    await expect(
      bobSubscription.units
    ).to.equal(
      "1"
    );

    // distribution SHOULD NOT REVERT if there are outstanding units issued
    await expect( spreader.connect(alice).distribute() ).to.be.not.reverted;

  });

  it("Distribution with [ 2 units issued to different accounts ] and [ 100 spreaderTokens ] - gainShare", async function () {

    let distributionAmount = ethers.utils.parseEther("100");

    //// ACTIONS

    // Admin gives spreader 100 DAIx
    const daiXTransferOperation = daix.transfer({
      receiver: spreader.address,
      amount: distributionAmount,
      providerOrSigner: admin
    })
    await daiXTransferOperation.exec(admin);

    // (snapshot balances)
    let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
    let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});

    // Distribution executed
    await expect( spreader.connect(admin).distribute() ).to.be.not.reverted;


    //// EXPECTATIONS

    // expect alice to receive 1/2 of distribution
    await expect(
      await daix.balanceOf({account: alice.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(aliceInitialBlance).add( distributionAmount.div("2") ), // expect original balance + distribution amount / 2
      expecationDiffLimit
    );

    // expect bob to receive 1/2 of distribution
    await expect(
      await daix.balanceOf({account: bob.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(bobInitialBlance).add( distributionAmount.div("2") ), // expect original balance + distribution amount / 2
      expecationDiffLimit
    );

    // expect balance of spreader contract to be zeroed out
    await expect(
      await daix.balanceOf({account: spreader.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from("0"),
      expecationDiffLimit
    );

  });

  it("Distribution with [ 3 units issued to different accounts ] and [ 100 spreaderTokens ] - gainShare", async function () {

    let distributionAmount = ethers.utils.parseEther("100");

    //// ACTIONS

    // Bob claims another distribution unit
    await spreader.connect(bob).gainShare(bob.address);

    // Admin gives spreader 100 DAIx
    const daiXTransferOperation = daix.transfer({
      receiver: spreader.address,
      amount: distributionAmount,
      providerOrSigner: admin
    })
    await daiXTransferOperation.exec(admin);

    // (snapshot balances)
    let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
    let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});

    // Distribution executed
    await expect( spreader.connect(admin).distribute() ).to.be.not.reverted;


    //// EXPECTATIONS

    // expect bob to have 2 distribution units
    let bobSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: bob.address, 
      providerOrSigner: bob 
    })

    await expect(
      bobSubscription.units
    ).to.equal(
      "2"
    );

    // expect alice to receive 1/3 of distribution
    await expect(
      await daix.balanceOf({account: alice.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(aliceInitialBlance).add( distributionAmount.div("3") ), // expect original balance + distribution amount * 1/2
      expecationDiffLimit
    );

    // expect bob to receive 2/3 of distribution
    await expect(
      await daix.balanceOf({account: bob.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(bobInitialBlance).add( (distributionAmount.div("3")).mul("2") ), // expect original balance + distribution amount * 2/3
      expecationDiffLimit
    );

    // expect balance of spreader contract to be zeroed out
    await expect(
      await daix.balanceOf({account: spreader.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from("0"),
      expecationDiffLimit
    );

  });

  it("Distribution with [ 2 units issued to single account ] and [ 100 spreaderTokens ] - deleteShares", async function () {

    let distributionAmount = ethers.utils.parseEther("100");

    //// ACTIONS

    // Alice deletes here entire subscription
    await spreader.connect(alice).deleteShares(alice.address);

    // Admin gives spreader 100 DAIx
    const daiXTransferOperation = daix.transfer({
      receiver: spreader.address,
      amount: distributionAmount,
      providerOrSigner: admin
    })
    await daiXTransferOperation.exec(admin);

    // (snapshot balances)
    let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
    let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});

    // Distribution executed
    await expect( spreader.connect(admin).distribute() ).to.be.not.reverted;


    //// EXPECTATIONS

    // expect alice to have 0 distribution units
    let aliceSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: alice.address, 
      providerOrSigner: alice 
    })

    await expect(
      aliceSubscription.units
    ).to.equal(
      "0"
    );

    // expect alice to receive none of distribution 
    await expect(
      await daix.balanceOf({account: alice.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(aliceInitialBlance), // expect original balance
      expecationDiffLimit
    );

    // expect bob to receive all of distribution
    await expect(
      await daix.balanceOf({account: bob.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(bobInitialBlance).add( distributionAmount ), // expect original balance + distribution amount
      expecationDiffLimit
    );

    // expect balance of spreader contract to be zeroed out
    await expect(
      await daix.balanceOf({account: spreader.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from("0"),
      expecationDiffLimit
    );

  });

  it("Distribution with [ 1 unit issued to single account ] and [ 100 spreaderTokens ] - loseShare", async function () {

    let distributionAmount = ethers.utils.parseEther("100");

    //// ACTIONS

    // Bob deletes one of his two units
    await spreader.connect(bob).loseShare(bob.address);

    // Admin gives spreader 100 DAIx
    const daiXTransferOperation = daix.transfer({
      receiver: spreader.address,
      amount: distributionAmount,
      providerOrSigner: admin
    })
    await daiXTransferOperation.exec(admin);

    // (snapshot balances)
    let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
    let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});

    // Distribution executed
    await expect( spreader.connect(admin).distribute() ).to.be.not.reverted;


    //// EXPECTATIONS

    // expect bob to have 1 distribution unit
    let bobSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: bob.address, 
      providerOrSigner: bob 
    })

    await expect(
      bobSubscription.units
    ).to.equal(
      "1"
    );

    // expect alice to receive none of distribution 
    await expect(
      await daix.balanceOf({account: alice.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(aliceInitialBlance), // expect original balance
      expecationDiffLimit
    );

    // expect bob to receive all of distribution
    await expect(
      await daix.balanceOf({account: bob.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(bobInitialBlance).add( distributionAmount ), // expect original balance + distribution amount
      expecationDiffLimit
    );

    // expect balance of spreader contract to be zeroed out
    await expect(
      await daix.balanceOf({account: spreader.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from("0"),
      expecationDiffLimit
    );

  });

  it("Distribution with [ no units outstanding ] and [ 100 spreaderTokens ] - loseShare", async function () {

    let distributionAmount = ethers.utils.parseEther("100");

    //// ACTIONS

    // Bob deletes his last unit
    await spreader.connect(bob).loseShare(bob.address);

    // Admin gives spreader 100 DAIx
    const daiXTransferOperation = daix.transfer({
      receiver: spreader.address,
      amount: distributionAmount,
      providerOrSigner: admin
    })
    await daiXTransferOperation.exec(admin);

    // (snapshot balances)
    let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
    let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});

    // distribution SHOULD REVERT since no units are outstanding
    await expect( spreader.connect(admin).distribute() ).to.be.reverted;


    //// EXPECTATIONS

    // expect bob to have no distribution units
    let bobSubscription = await sf.idaV1.getSubscription({ 
      superToken: daix.address, 
      publisher: spreader.address, 
      indexId: "0", // recall this was `INDEX_ID` in TokenSpreader.sol
      subscriber: bob.address, 
      providerOrSigner: bob 
    })

    await expect(
      bobSubscription.units
    ).to.equal(
      "0"
    );

    // expect alice to receive none of distribution 
    await expect(
      await daix.balanceOf({account: alice.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(aliceInitialBlance), // expect original balance
      expecationDiffLimit
    );

    // expect bob to receive none of distribution
    await expect(
      await daix.balanceOf({account: bob.address, providerOrSigner: admin})
    ).to.closeTo(
      ethers.BigNumber.from(bobInitialBlance), // expect original balance
      expecationDiffLimit
    );

    // expect balance of spreader contract to remain same
    await expect(
      await daix.balanceOf({account: spreader.address, providerOrSigner: admin})
    ).to.closeTo(
      distributionAmount,
      expecationDiffLimit
    );

  });

});