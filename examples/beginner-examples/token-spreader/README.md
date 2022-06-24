# IDA Tutorial

Hey buidler! 👋

Here’s the place to get started building with Superfluid’s [Instant Distribution Agreement](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview/super-agreements/instant-distribution-agreement-ida) (IDA). After completion of this guide, you’ll be up and running with the basics of creating smart contract’s that leverage scalable 1-to-many distributions and interact with the Superfluid Core SDK to engage with the IDA.

This guide is meant to service developers of all skill levels, but we recommend having some competence with Solidity and Javascript before getting started. We also recommend reading through our In-Depth [Protocol Overview](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview), and looking through our [Interactive Tutorials](https://docs.superfluid.finance/superfluid/developers/interactive-tutorials) in the docs.

# **What is the Instant Distribution Agreement (IDA)?**

**But before going any further, we highly recommend you go through this [conceptual breakdown](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview/super-agreements/instant-distribution-agreement-ida) from our docs that thoroughly explains the IDA**. This will get you comfortable with the terminology and core concepts which you’ll apply through code in this tutorial.

But, for a bite-sized ELI5...

In an IDA, an account (the "publisher") can send Super Tokens to any amount of receivers (called "subscribers") on a proportional basis with *fixed gas costs*.

Basically, the publisher gives distribution units to these various subscribers. The more distribution units a subscriber has, the more of the distribution it will receive.

For instance, if there were 3 outstanding units, 1 held by account A and 3 held by account B, then if 100 USDCx were sent out, then account A would receive 25 USDCx and account B would receive 75 USDCx. Again, for an even better explainer, check out [this page](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview/super-agreements/instant-distribution-agreement-ida) in the docs.

---

# Token Spreader Example

To help demo using the IDA in a smart contract, we’re going to write a ✨ **Token Spreader contract** ✨

The Token Spreader will be a ultra-simple contract that takes tokens transferred into it and disperses them to accounts holding distribution units. Any account is free to gain individual units, lose individual units, or delete all their units using the TokenSpreader contract’s methods.

## Fire up a Hardhat Project and Get Started!

1. [Create a fresh Hardhat project](https://hardhat.org/getting-started)
2. Install Superfluid’s Ethereum contracts with `npm install @superfluid-finance/ethereum-contracts`
3. Crack open the contracts folder and rename what’s there to TokenSpreader.sol
4. Let’s write some Solidity!

## Imports

```solidity
pragma solidity 0.8.13;

import { ISuperfluid, ISuperToken } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/superfluid/ISuperfluid.sol";

import { IInstantDistributionAgreementV1 } from "@superfluid-finance/ethereum-contracts/contracts/interfaces/agreements/IConstantFlowAgreementV1.sol";

import { IDAv1Library } from "@superfluid-finance/ethereum-contracts/contracts/apps/IDAv1Library.sol";
```

**Terms and Snippets**

**`ISuperToken`**

Allows us to use Super Tokens within our contract

**`ISuperfluid`**

The [Superfluid Host Contract](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview/superfluid-host)

**`IInstantDistributionAgreementV1`**

The IDA Super Agreement interface that will facilitate instant distributions

**`IDAv1Library`**

A helper library that will allow us to easily interact with and manage the contract’s distributions

## State Variables

```solidity
contract TokenSpreader {

    ISuperToken public spreaderToken;                  // Token to be distributed to unit holders by distribute() function

    using IDAv1Library for IDAv1Library.InitData;      // Creating idaV1 object for easy use of IDA functions
    IDAv1Library.InitData public idaV1;

    uint32 public constant INDEX_ID = 0;               // The IDA Index. Since this contract will only use one index, we'll hardcode it to "0".

...
```

**Terms and Snippets**

**`spreaderToken`**

This is the Super Token that will be supported for the TokenSpreader’s distribution index. Remember, each distribution index can only support one Super Token.

**`idaV1`**

This is the object with which the contract can interact in order to call Instant Distribution functions.

**`INDEX_ID`**

An account or contract may have multiple IDA Indices. Each IDA Index has it’s own Index ID which is set when it’s created. Here, we’re hardcoding it to “0” because the TokenSpreader contract will only be using one IDA Index.

## Constructor

```solidity
constructor(
        ISuperfluid _host,
        ISuperToken _spreaderToken
    ) {

        // Ensure _spreaderToken is indeed a super token
        require(address(_host) == _spreaderToken.getHost(),"!superToken");

        spreaderToken = _spreaderToken;

        // Initializing the host and agreement type in the idaV1 object so the object can have them on hand for enacting IDA functions
        // Read more on initialization: https://docs.superfluid.finance/superfluid/developers/solidity-examples/solidity-libraries/idav1-library#importing-and-initialization
        idaV1 = IDAv1Library.InitData(
            _host,
            IInstantDistributionAgreementV1(
                address(_host.getAgreementClass(keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1")))
            )
        );

        // Creates the IDA Index through which tokens will be distributed
        idaV1.createIndex(_spreaderToken, INDEX_ID);

    }
```

**Terms and Snippets**

**`_host`**

The address of the [Superfluid Host Contract](https://docs.superfluid.finance/superfluid/protocol-overview/in-depth-overview/superfluid-host) which is in turn used to instantiate the `IDAv1Library`

**`_spreaderToken`**

The supported Super Token

**`idaV1.createIndex(_spreaderToken, INDEX_ID);`**

Now the contract has an IDA Index for `spreaderToken` identified with `INDEX_ID`. It can use this Index to start doing cool Instant Distribution stuff.

**`require(address(_host) == _spreaderToken.getHost(),"!superToken");`**

Every Super Token gets registered with the Host contract upon deployment. If no address or the wrong address is returned when getting the host from the `spreaderToken` passed in, we know we don’t have a proper Super Token on our hands and our contract won’t function properly. Hence, we verify it with this require statement.

```solidity
idaV1 = IDAv1Library.InitData(
      _host,
      IInstantDistributionAgreementV1(
          address(_host.getAgreementClass(keccak256("org.superfluid-finance.agreements.InstantDistributionAgreement.v1")))
      )
);
```

The code where we use host.getAgreementClass and pass in the hash of that link to the constant flow agreement allows us to get the address of the currently deployed IDA on our network without needing to pass it in as a variable to the constructor. We recommend using this pattern as well.

## Distribute Function

This function will take its entire `spreaderToken` Super Token balance and distribute it out to unit holders.

- Notice the use of `calculateDistribution()`. This allows us to get an amount to distribute that will avoid any rounding errors.
- By default of `idaV1.distribute`, this function will revert if there have been no units issued.

```solidity
		/// @notice Takes the entire balance of the designated spreaderToken in the contract and distributes it out to unit holders w/ IDA
    function distribute() public {

        uint256 spreaderTokenBalance = spreaderToken.balanceOf(address(this));

        (uint256 actualDistributionAmount,) = idaV1.ida.calculateDistribution(
            spreaderToken,
            address(this),
            INDEX_ID,
            spreaderTokenBalance
        );

        idaV1.distribute(spreaderToken, INDEX_ID, actualDistributionAmount);

    }
```

## Unit-Modifying Functions

Here is the functionality we introduce where accounts can work with their shares of the TokenSpreader’s IDA Index. The pattern here (except for delete), is to get the current amount of subscription units and increment or decrement accordingly.

```solidity
		/// @notice lets an account gain a single distribution unit
    /// @param subscriber subscriber address whose units are to be incremented
    function gainShare(address subscriber) public {

        // Get current units subscriber holds
        (,,uint256 currentUnitsHeld,) = idaV1.getSubscription(
            spreaderToken,
            address(this),
            INDEX_ID,
            subscriber
        );

        // Update to current amount + 1
        idaV1.updateSubscriptionUnits(
            spreaderToken,
            INDEX_ID,
            subscriber,
            uint128(currentUnitsHeld + 1)
        );

    }

    /// @notice lets an account lose a single distribution unit
    /// @param subscriber subscriber address whose units are to be decremented
    function loseShare(address subscriber) public {

        // Get current units subscriber holds
        (,,uint256 currentUnitsHeld,) = idaV1.getSubscription(
            spreaderToken,
            address(this),
            INDEX_ID,
            subscriber
        );

        // Update to current amount - 1 (reverts if currentUnitsHeld - 1 < 0, so basically if currentUnitsHeld = 0)
        idaV1.updateSubscriptionUnits(
            spreaderToken,
            INDEX_ID,
            subscriber,
            uint128(currentUnitsHeld - 1)
        );

    }

    /// @notice allows an account to delete its entire subscription this contract
    /// @param subscriber subscriber address whose subscription is to be deleted
    function deleteShares(address subscriber) public {

        idaV1.deleteSubscription(
            spreaderToken,
            address(this),
            INDEX_ID,
            subscriber
        );

    }
```

---

# Setting up Hardhat Config and Environment

Before we try out testing and scripting, we need to get our config and environment set. Let’s start with the environment.

**Environment File**

Referring to the `.env.template` file and fill out the fields in your `.env` file

- `GOERLI_URL` - Put in an RPC Endpoint (I recommend Infura, here are [the steps](https://blog.infura.io/post/getting-started-with-infura-28e41844cc89) on how to get one from them.)
- `MNEMONIC` - Make sure you’re not using a seed phrase for a wallet that has any real money on it!
- `ETHERSCAN_API_KEY` - Get one [here](https://etherscan.io/apis), and just use the free option.
- `TOKENSPREADER_ADDRESS` - Leave blank for now. We’ll fill this in when we deploy the TokenSpreader to Goerli later.

**Hardhat Config**

Make sure your imports look like this:

```jsx
require("@nomiclabs/hardhat-etherscan");
require("@nomiclabs/hardhat-truffle5");
require("@nomiclabs/hardhat-ethers");
require("@nomiclabs/hardhat-waffle");
require("hardhat-gas-reporter");
require("solidity-coverage");
```

Set up the `networks` section as such:

```jsx
...
networks: {
  goerli: {
    url: process.env.GOERLI_URL || "",
    accounts: {
      mnemonic: process.env.MNEMONIC,
      initialIndex: 0,
      count: 10,
    }
  },
},
...
```

Increase the testing timeout with:

```jsx
mocha: {
    timeout: 50000000 // setting it very high so testing doesn't complain
}
```

# Setting Up the Test Suite

Great, so our smart contract and our environment is set up. Let’s get to work testing it! Normally, we’d want to practice test-driven development (TDD) where we write the tests first and then write the Solidity after, but this sequence makes for a nicer tutorial 😄

Let’s walk through the steps needed to get set testing our TokenSpreader contract before we start fleshing out the test script logic.

View on Github 👇

[protocol-monorepo/TokenSpreader.test.js at simple_ida_example_tweak · superfluid-finance/protocol-monorepo](https://github.com/superfluid-finance/protocol-monorepo/blob/simple_ida_example_tweak/examples/token-spreader/test/TokenSpreader.test.js)

## Installations

Run the below to install the needed Superfluid dependencies.

`npm install @nomiclabs/hardhat-truffle5 @nomiclabs/hardhat-ethers @superfluid-finance/sdk-core`

## Imports and State Variables

At the top of our test file, we’ll include our imports and also set up several state variables which will be modified throughout our test suite. We’ve also imported the `deployFramework`, `deployFramework`, and `deployFramework` scripts which will help us kick off Superfluid testing locally.

```jsx
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
...
```

## before() Hook

When setting up a Hardhat test script to test locally and do Superfluid things, we’ll need to:

1. Deploy the Superfluid Protocol locally with the `deployFramework` script. Here we set our Superfluid SDK Core Framework object (`sf`) which will be out one-stop shop for Superfluid interactions.

```jsx
...
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

...
```

2. Deploy a Test ERC20 Token and Wrapper Super Token around that ERC20 token locally with the `deployTestToken` and `deploySuperToken` scripts. We’ll pretend we’re working with DAI here. Notice that we use the `TestTokenABI` that we imported earlier. We’re using this because the these test tokens have an overridden `mint` function which allows anyone to mint more of them, which you’ll see used in the next step.

```jsx

...
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
...
```

3. Set up each testing account with Super Tokens.

Notice that the upgrade transactions (which wrap the DAI into DAIx) **are not Ethers.js syntax** in that they don’t follow the `[contract object].connect([signer object]).[function]` syntax. Rather they define a transaction object and allow it to be executed with `.exec(signer)`. The reason is that `daix` object here is not a basic Ethers.js contract object; it’s a special Super Token object created via the deploy scripts we used.

```jsx
...
		//// SETTING UP NON-ADMIN ACCOUNTS WITH DAIx

    // minting test DAI
    await dai.connect(admin).mint(admin.address, ethers.utils.parseEther("10000"));
    await dai.connect(alice).mint(alice.address, ethers.utils.parseEther("10000"));
    await dai.connect(bob).mint(bob.address, ethers.utils.parseEther("10000"));

    // approving DAIx to spend DAI (Super Token object is not an ethers contract object and has different operation syntax)
    await dai.connect(admin).approve(daix.address, ethers.constants.MaxInt256);
    await dai.connect(alice).approve(daix.address, ethers.constants.MaxInt256);
    await dai.connect(bob).approve(daix.address, ethers.constants.MaxInt256);

    // Wrapping all DAI into DAIx
    const daiXUpgradeOperation = daix.upgrade({
      amount: ethers.utils.parseEther("10000").toString(),
    })
    await daiXUpgradeOperation.exec(admin);
    await daiXUpgradeOperation.exec(alice);
    await daiXUpgradeOperation.exec(bob);
...
```

4. Deploy the TokenSpreader contract

```jsx
	...
		//// INITIALIZING SPREADER CONTRACT

    const spreaderContractFactory = await ethers.getContractFactory(
        "TokenSpreader",
        admin
    );

    spreader = await spreaderContractFactory.deploy(
        sf.settings.config.hostAddress,
        daix.address, // Setting DAIx as spreader token
    );
...
```

5. Have each testing account subscribe to the TokenSpreader’s IDA Index.

This way the Super Tokens that TokenSpreader distributes actually show up in each testing account. For simplicity sake, if an account is not subscribed, but receives a distribution, its tokens will essentially “hang in limbo” until the account subscribes, after which they will go through. We’re just subscribing right after deployment to keep things simple.

```jsx
...
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
```

# Unit Testing

So, you want to make sure that after certain actions have been done to the contract and units have been issued/deleted/etc., that once `distribute()` is called, everyone’s getting the right amount of tokens.

Here we’ll walk through a single easy unit test that paints the general structure of testing for that.

**The Structure**

1. Run Actions: `gainShare()`, `loseShare()`, `deleteShares()`, transfers, etc.

```jsx
// Assume earlier in the testing, Alice and Bob each called gainShare() once,
// So, we expect them each to have 1 distribution unit at the start of this unit test

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
...
```

2. Snapshot initial balances of testing accounts

```jsx
		...
			// (snapshot balances)
		  let aliceInitialBlance = await daix.balanceOf({account: alice.address, providerOrSigner: admin});
		  let bobInitialBlance = await daix.balanceOf({account: bob.address, providerOrSigner: admin});
```

3. Run distribution

```jsx
		...
			// Distribution executed
			await expect( spreader.connect(admin).distribute() ).to.be.not.reverted;
		...
```

4. Check expectations. Notice the `expecationDiffLimit`. This was set because sometimes the IDA outputs a few wei less than what’s expected, so 10 wei of leeway is introduced as you saw set in the [Imports and State Variables](https://www.notion.so/IDA-Tutorial-80266bdd687f4293bb7066cd4ed8c9f6) section.

```jsx
...
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
```

It’s too much for this tutorial to walk through the entire test suite. So having learned how we structure these tests, I’d recommend just stepping through the whole unit test suite in the [test file](https://github.com/superfluid-finance/protocol-monorepo/blob/dev/examples/token-spreader/test/TokenSpreader.test.js).

---

# Deploying and using the TokenSpreader with Hardhat scripts

Ok, so we’ve got our TokenSpreader set and we’ve written some tests. Let’s hop in the **scripts** folder get to work deploying it to Goerli and then using some scripts to interact with them.

View on Github 👇

https://github.com/superfluid-finance/protocol-monorepo/blob/simple_ida_example_tweak/examples/token-spreader/scripts/deploy.js

## Deploy

**1. Deploy on Goerli**

Head to the `deploy.js` in the scripts folder.

In the code, notice how with set up the Superfluid Framework object and used it to load the fDAIx Super Token. This is simple best practice as we get to use `loadSuperToken()` to pull the Super Token’s info straight from the Resolver inside the framework. The Resolver is like an internal compilation of useful Superfluid info.

```jsx
...
// Setting up network object - this is set as the goerli url, but can be changed to reflect your RPC URL and network of choice
  const url = `${process.env.GOERLI_URL}`;
  const customHttpProvider = new ethers.providers.JsonRpcProvider(url);
  const network = await customHttpProvider.getNetwork();

  // Setting up the out Framework object with Goerli (knows it's Goerli when we pass in network.chainId)
  const sf = await Framework.create({
    chainId: network.chainId,
    provider: customHttpProvider,
    customSubgraphQueriesEndpoint: "",
    dataMode: "WEB3_ONLY"
  });

  // Getting the Goerli fDAIx Super Token object from the Framework object
  // This is fDAIx on goerli - you can change this token to suit your network and desired token address
  const daix = await sf.loadSuperToken("fDAIx");
...
```

Hardhat lets you run scripts with the `npx hardhat run scripts/[script JS file] --network [network name from hardhat config]` format.

So here, you’d run the deploy with `**npx hardhat run scripts/deploy.js --network goerli**`

**2. Verify on Etherscan**

Head to `arguments-tokenspreader.js` in the base of the repository and notice how the arguments you want to pass into the TokenSpreader constructor are hardcoded.

Using the contract address from the previous step, run the following:

`npx hardhat verify --network goerli --constructor-args arguments-tokenspreader.js [contractaddress]`

You’ll now be able to see your contract verified on Etherscan! Here’s an example of one that I verified:

https://goerli.etherscan.io/address/0x48Cf61C16D066ad597e390E7e575E8832e58BddA#writeContract

**3. Add to Environment File**

Go to your .env file and set the `TOKENSPREADER_ADDRESS` to the address from deployment

## Give Out Shares

Head to `gainShare.js`.

It’s a script that takes one of four addresses from your seed phrase and gives it a distribution unit by calling `gainShare()` on the TokenSpreader contract. It also subscribes the account to the TokenSpreader’s distribution unit so that upon distribution, the spreaderTokens go straight to the account!

On the `let shareGainer =` you can specify the account that will be gaining the share. Let’s give each of the four accounts a unit.

Run it with `**npx hardhat run scripts/gainShare.js --network goerli**`

## Give TokenSpreader some spreaderTokens

Head to `transferInSpreaderToken.js`.

The spreaderToken we designated in deployment was fDAIx. This script mints some fDAI (hardcoded to 1,000 but you can change the amount), wraps it into fDAIx and send it all into the TokenSpreader contract.

Run it with `**npx hardhat run scripts/transferInSpreaderToken.js --network goerli**`

## Distribute out the spreaderTokens

Run the viewStatus.js script with `**npx hardhat run scripts/viewStatus.js --network goerli**`. You’ll see that all four addresses have a distribution unit and the TokenSpreader’s spreaderToken Balance is loaded at 1,000.

Run the distribution with `**npx hardhat run scripts/distribute.js --network goerli`.**

Now, run the viewStatus.js script again and you’ll see that TokenSpreader’s spreaderToken Balance has been zeroed out and the four addresses have each gained their share of the 1,000 token distribution!

![Image](https://user-images.githubusercontent.com/62968241/174394290-8b03e3fd-6d5d-4cd0-ad79-795f03857671.png)

# That’s all, folks!

Through this tutorial, you have:

- Written a TokenSpreader Solidity smart contract that uses the Instant Distribution Agreement
- Learned how to test the distribution functionality of the contract with Ethers.js and the Superfluid SDK Core
- Write scripts to deploy and interact with the TokenSpreader contract on Goerli using Ethers.js and SDK Core.

Way to go! 🥳

If you’re up to another challenge, try further testing your comprehension of the IDA with the [Dividend Rights Token](https://github.com/superfluid-finance/protocol-monorepo/blob/dev/examples/rewards-distribution-token/contracts/DividendRightsToken.sol) example which tokenizes IDA units into ERC20 tokens.
