 # The Rewards Distribution token
 This project demonstrates an ERC20 token that tokenizes units in Superfluid [Instant Distribution Agreements](https://docs.superfluid.finance/superfluid/protocol-developers/interactive-tutorials/instant-distribution). 

### Steps for running the project:
1) Install deps with `yarn install`.
2) Create your own _.env_ file based (see _.env.example_).
3) Make sure the signer account (defined by the PRIVATE_KEY env var) has native tokens for paying tx fees on the configured network.
4) Set the network config in _hardhat.config.ts_. For goerli, you can just uncomment the relevant lines already in place.
5) Run ```yarn build``` to compile and generate typings for the Dividend Rights Token contract.
6) Run ```npx hardhat run scripts/deploy.ts``` to deploy the contract.

# Advanced Sample Hardhat Project

This project demonstrates an advanced Hardhat use case, integrating other tools commonly used alongside Hardhat in the ecosystem.

The project comes with a sample contract, a test for that contract, a sample script that deploys that contract, and an example of a task implementation, which simply lists the available accounts. It also comes with a variety of other tools, preconfigured to work with the project code.

Try running some of the following tasks:

```shell
npx hardhat accounts
npx hardhat compile
npx hardhat clean
npx hardhat test
npx hardhat node
npx hardhat help
REPORT_GAS=true npx hardhat test
npx hardhat coverage
npx hardhat run scripts/deploy.ts
TS_NODE_FILES=true npx ts-node scripts/deploy.ts
npx eslint '**/*.{js,ts}'
npx eslint '**/*.{js,ts}' --fix
npx prettier '**/*.{json,sol,md}' --check
npx prettier '**/*.{json,sol,md}' --write
npx solhint 'contracts/**/*.sol'
npx solhint 'contracts/**/*.sol' --fix
```

# Etherscan verification

To try out Etherscan verification, you first need to deploy a contract to an Ethereum network that's supported by Etherscan, such as Görli.

In this project, copy the .env.example file to a file named .env, and then edit it to fill in the details. Enter your Etherscan API key, your RPC node URL (eg from Alchemy), and the private key of the account which will send the deployment transaction. 
Then make sure there's a section for the etherscan config in _hardhat.config.ts_ (uncomment it).

With that in place, first deploy your contract:

```shell
hardhat run scripts/deploy.ts
```

If succeeding, this will print the command needed for etherscan verification as the last output.  
Execute that command!

The general syntax is:

```shell
npx hardhat verify DEPLOYED_CONTRACT_ADDRESS <constructor arguments...>
```

# Performance optimizations

For faster runs of your tests and scripts, consider skipping ts-node's type checking by setting the environment variable `TS_NODE_TRANSPILE_ONLY` to `1` in hardhat's environment. For more details see [the documentation](https://hardhat.org/guides/typescript.html#performance-optimizations).
