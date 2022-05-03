# Simple ACL Close Example

## About
A super simple foundry/Hardhat hybrid project which utilizes Superfluid's CFA Access Control List feature and Gelato to close a flow at a predetermined time in the future.

## Built With

- [@superfluid-finance/ethereum-contracts](https://www.npmjs.com/package/@superfluid-finance/ethereum-contracts)
- [@superfluid-finance/sdk-core](https://www.npmjs.com/package/@superfluid-finance/sdk-core)
- [foundry](https://github.com/foundry-rs/foundry)
- [Hardhat](https://hardhat.org/)
- [Gelato Ops](https://app.gelato.network/)

## Prerequisites
In order to run this project, you need to have the following dependencies installed on your computer:

- [foundry](https://github.com/foundry-rs/foundry)
- [yarn](https://yarnpkg.com/getting-started/install) or you can just use npm, but you'll need to change up the `Makefile` slightly. 

## Project Setup
To set up the project run the following command:
```
make
```

This runs the `all:` command which does the following:

- `clean`: removes the `/out` and `/cache` files that are generated when you run `forge build`
- `remove`: removes gitmodules and lib and recreates this
- `install`: installs the required modules and generates the remappings.txt file for VSCode integration
- `setup-yarn`: installs package dependencies inside `package.json`
- `update`: updates forge dependencies
- `solc`: installs the specified solc version
- `build`: builds the contracts

> NOTE: You can also run these separately, e.g. `make install`

## Deployment and Interaction w/ Hardhat Console
Open a terminal window and start up a local hardhat node: `yarn hardhat-node` or `npx hardhat node`.

Open another terminal window and deploy the contracts locally: `yarn deploy <NETWORK_NAME>` or `npx hardhat run scripts/deploy.ts --network <NETWORK_NAME>`

Start a hardhat console session: `yarn console <NETWORK_NAME>` or `npx hardhat console --network <NETWORK_NAME>`

Attach the newly deployed contract:

`const simpleACLCloseResolver = await hre.ethers.getContractAt("SimpleACLCloseResolver", <CONTRACT_ADDRESS>);`

Now you can interact with the contract in the CLI.

> NOTE: Use localhost as `<NETWORK_NAME>` if you want to deploy locally.

## Contract Verification
Contract verification occurs by default after deploying via Hardhat. This will require multiple retries due to the bytecode not being deployed until after a certain number of confirmations has gone through.

To verify a contract manually via the CLI, we need to use a custom created Hardhat task, passing in the constructor args:
```
yarn hardhat-verify --address <CONTRACT_ADDRESS> --args '[<END_TIME>, "<CFA_ADDRESS>", "<SUPERTOKEN_ADDRESS>", "<SENDER_ADDRESS>", "<RECEIVER_ADDRESS>"]'
```
> NOTE: You can also set the SKIP_VERIFICATION variable to true in `.env` to skip the contract verification process after contract deployment.

## Tests
To test with Hardhat:
1. Set up a local hardhat node:
`npx hardhat node`

2. Run the tests
`yarn hardhat-test --network localhost` or `npx hardhat test --network localhost`

3. Coverage tests (optional)
`yarn coverage`

To test with Forge: `forge test`

## TDD (Test Driven Development)
To have a TDD workflow, we use nodemon to track changes to .ts/.sol files for hardhat driven test development or .sol files for forge driven test development.
Forge driven test development command: `yarn forge-dev`
Hardhat driven test development command: `yarn hardhat-dev`

## Coverage
To get a coverage report, run `yarn coverage` or `npx hardhat coverage`.

## Steps
To get this up and running and helping you close a stream, here is a step-by-step guide:

0. Create a flow using the [dashboard](https://app.superfluid.finance) or using one of the SDK's or a custom script. You will use some of this information when deploying the resolver contract.

1. Deploy the gelato resolver contract.

You can do this with forge or Hardhat.

You can get your desired end time by creating a date object in javascript and then calling `.getTime()` to get the time in milliseconds and dividing by 1000 to convert that to seconds.

Forge command: `make deploy`

Hardhat command: `yarn deploy <NETWORK_NAME>` or `npx hardhat run scripts/deploy.ts --network <NETWORK_NAME>`

> NOTE: You will have to provide/update the necessary .env values for this to work.

(optional) Verify your contract - this makes it slightly easier when setting up the task on gelato ops.
If you don't verify, you just need to copy the ABI into the input when prompted for it when setting up your task with Gelato. Refer above for the commands on contract verification.

2. Authorize delete control to the flow operator (gelato ops).

```
npx hardhat run scripts/authorizeControl.ts --network <NETWORK_NAME>
```

3. Set up the Gelato Ops
 - Click `Create Task`
 - Copy and paste the [address](https://docs.superfluid.finance/superfluid/protocol-developers/networks) of Superfluid Host contract into the `Execute` contract address input
 - Select `callAgreement(address agreementClass, bytes callData, bytes userData)` as the `Function to be automated`
 - Click `Dynamic inputs via Resolver` and paste the address of where your resolver was deployed to into the `Resolver contract address` input
 - Select `checker()` as the `Function to be called at the resolver (to check task status)`
 - Give the task a name and create it. 

 > Note: You will need to top up some funds in Gelato for the executor to execute transactions on your behalf.

## Next Steps
The written example is only for closing streams, but you can easily modify the resolver to:

 - Automatically upgrade tokens for your account when your supertoken balance is at a certain amount
 - Handle managing multiple streams