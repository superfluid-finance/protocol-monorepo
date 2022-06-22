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