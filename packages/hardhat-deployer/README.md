# @superfluid-finance/hardhat-deployer

This plugin brings a Superfluid framework deployer for development environments using `ethers`.

> **NOTICE**: This is not suitable for public deployments.
> This is designed for local testing andd requires unusually high gas limits.

## What does it do?

The `@superfluid-finance/hardhat-deployer` plugin handles deployment and initialization of Superfluid contracts in a
development environment.

## Installation

```bash
npm install @superfluid-finance/hardhat-deployer @superfluid-finance/ethereum-contracts
```

Import the plugin in `hardhat.config.js`:

```js
require("@superfluid-finance/hardhat-deployer");
```

Or in a Typescript file, use the following in `hardhat.config.ts`:

```ts
import "@superfluid-finance/hardhat-deployer";
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
