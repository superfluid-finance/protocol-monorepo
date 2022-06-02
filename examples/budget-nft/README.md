# Budget NFT
#### A framework for DAO cash flow management using money streaming and NFTs

If you'd like a step by step walkthrough on how to use this example, [this workshop](https://www.youtube.com/watch?v=alnIRuoKocw&t=1484s) is a great guide.

The main Budget NFT smart contract will enable you to
- Mint NFTs to various working groups or DAO contributors that come attached with money streams
- Transferring these NFTs will result in the transfer of the money stream to the new recipient
- FlowRates may be edited by the owner of the core BudgetNFT contract (ideally the DAO itself)
- Owners of each individual NFT may split their NFT into separate NFTs
- NFTs may be merged together if they fall under the ownership of the same account

Note: we suggest building upon this. It is strictly meant for example purposes only.

## Using This Example

You can run the following tasks to make use of this example

#### Basic Commands:
Testing: `yarn test`
Compilation: `yarn build`

#### Project Setup
Create a `.env` file based on the `.env.template`. Add your own RPC URL and private key for your deployer in that file.
Make sure that the `hardhat.config.ts` file is up to date with the network you want to use. Note: you should follow this syntax when adding a network to the 'Networks' list in `hardhat.config.ts`
  ```
  networks: {
    ropsten: {
      url: process.env.GOERLI_URL || "",
      accounts:
        process.env.PRIVATE_KEY !== undefined ? [process.env.PRIVATE_KEY] : [],
    },
  },
  ```

Deployment: `npx hardhat run scripts/deploy.js`
Fund Contract: `npx hardhat run scripts/fundContract.js`
Issue NFTs: `npx hardhat run scripts/issueNFT.js`
- Note: Make adjustments to the issueNFT script by adding your own parameters. Remember that your contract must have a positive super token balance first before you issue an NFT. Funds are going to be sent from the contract itself. 
Edit NFTs: `npx hardhat run scripts/editNFT.js`
- Note: Make adjustments to the editNFT script by adding your own parameters.
Split NFTs: `npx hardhat run scripts/splitNFT.js`
- Note: Make adjustments to the splitNFT script by adding your own parameters.
Merge NFTs: `npx hardhat run scripts/mergeNFTs.js`
- Note: Make adjustments to the mergeNFTs script by adding your own parameters. Remember that before you merge NFTs, you must make sure that both of these NFTs are under the same ownership.

We highly recommend making use of the [Superfluid Developer Console](https://console.superfluid.finance) and [Superfluid Dashboard](https://app.superfluid.finance) when working with examples like this. They will make your life easier.


# Advanced Sample Hardhat Project

This project demonstrates an advanced Hardhat use case, integrating other tools commonly used alongside Hardhat in the ecosystem.

The project comes with a sample contract, a test for that contract, a sample script that deploys that contract, and an example of a task implementation, which simply lists the available accounts. It also comes with a variety of other tools, preconfigured to work with the project code.
