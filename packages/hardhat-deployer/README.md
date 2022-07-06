# hardhat-superfluid

This plugin brings a Superfluid framework deployer for development environments using `ethers`.

> **NOTICE**: This is not suitable for public deployments.
> This is designed for local testing andd requires unusually high gas limits.

## What does it do?

The `hardhat-superfluid` plugin handles deployment and initialization of Superfluid contracts in a
development environment.

## Installation

```bash
npm install hardhat-superfluid @superfluid-finance/ethereum-contracts
```

Import the plugin in `hardhat.config.js`:

```js
require("hardhat-superfluid");
```

Or in a Typescript file, use the following in `hardhat.config.ts`:

```ts
import "hardhat-superfluid";
```

## Required plugins

-   [@nomiclabs/hardhat-ethers](https://github.com/nomiclabs/hardhat/tree/master/packages/hardhat-ethers)

## Tasks

This plugin creates no additional tasks.

## Environment extensions

This plugin extends the Hardhat Runtime Environment by adding the `superfluidFrameworkDeployer`
object, of type `SuperfluidFrameworkDeployer`. See "Usage" for details.

## Usage

> **NOTICE**: The deployments' gas limits are set to 6 million each. The default Hardhat network
> configuration accomodates this, but if you use other local networks such as Anvil, you will need to
> manually set the network gasLimit to at least 6 million.

Once the dependency is installed and imported into the Hardhat configuration file, you can import
the deployer into your test file as follows.

```js
const { superfluidFrameworkDeployer } = require("hardhat");
```

Or in a Typescript file, use the following.

```ts
import { superfluidFrameworkDeployer } from "hardhat";
```

You can now use the deployer to deploy the framework, as well as test tokens. It is recommended to
deploy these contracts before any unit tests are run.

```ts
// Note that `ethers` can be imported, assuming the `hardhat-ethers` dependency has been installed
// and imported into the Hardhat configuration file.
import { ethers, superfluidFrameworkDeployer } from "hardhat";

describe("Super App Unit Tests", async function () {
    let deployer;

    before(async function () {
        [deployer] = await ethers.getSigners();

        // This deploys the entire framework.
        // This MUST happen before deploying Super Tokens.
        await superfluidFrameworkDeployer.deploy(deployer);

        // This deploys an ERC20 token and a Wrapper Super Token
        // The `deployer` has mint permission on the ERC20
        const { underlyingToken: fDAI, superToken: fDAIx } =
            await superfluidFrameworkDeployer.deployMockSuperToken("Fake DAI", "fDAI", deployer);

        // Mint 100 Fake DAI to `deployer`
        await fDAI.mint(deployer.address, ethers.utils.parseEther("100"));

        // Approve Super Fake DAI for Upgrade
        await fDAI.approve(fDAIx.address, ethers.constants.MaxUint256);

        // Upgrade 100 Fake DAI to 100 Super Fake DAI
        await fDAIx.upgrade(ethers.utils.parseEther("100"));
    });
});
```

## Internals

### Deploy

The `deploy` function does the following, in order.

1. Deploy the `ERC1820Registry` to its appropriate address (if not deployed)
2. Deploy `Resolver`
3. Deploy `TestGovernance`
4. Register `TestGovernance` with `Resolver`
5. Deploy `Superfluid`
6. Initialize `Superfluid` with `TestGovernance`
7. Register `Superfluid` with `Resolver`
8. Initialize `TestGovernance`
9. Deploy `ConstantFlowAgreementV1`
10. Register `ConstantFlowAgreementV1` with `TestGovernance`
11. Deploy `SlotsBitmapLibrary`
12. Deploy `InstantDistributionAgreementV1` with link to `SlotsBitmapLibrary`
13. Register `InstantDistributionAgreementV1` with `TestGovernance`
14. Deploy `SuperTokenFactoryHelper`
15. Deploy `SuperTokenFactory`
16. "Update Code" with `TestGovernance`, registering the `SuperTokenFactory` with `Superfluid`
17. Return the `Resolver` address

### DeployMockSuperToken

The `deployMockSuperToken` function does the following, in order.

1. Deploy `ERC20PresetMinterPauser` with mint and pause permission to the `deployer`
2. Calls `createERC20Wrapper` on the `SuperTokenFactory` with the `ERC20PresetMinterPauser`
3. Returns both in a Javascript Object
