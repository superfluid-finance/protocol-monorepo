<h1 align="center">Welcome to Superfluid Subgraph 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="https://github.com/superfluid-finance/protocol-monorepo/raw/dev/sf-logo.png" />
<p>
  <a href="https://www.npmjs.com/package/@superfluid-finance/js-sdk" target="_blank">
    <img alt="Version" src="https://img.shields.io/npm/v/@superfluid-finance/js-sdk.svg">
  </a>
  <a href="#" target="_blank">
    <img alt="License: MIT" src="https://img.shields.io/badge/License-MIT-yellow.svg" />
  </a>
  <a href="https://twitter.com/Superfluid_HQ/status/" target="_blank">
    <img alt="Twitter: Superfluid_HQ" src="https://img.shields.io/twitter/follow/Superfluid_HQ.svg?style=social" />
  </a>
</p>
</div>

> Official subgraph for the Superfluid Protocol

# 📖 Docs

Get started using the Subgraphs with an introduction and query examples:

https://docs.superfluid.finance/superfluid/docs/subgraph

# 👨‍🚀 Hosted Subgraphs

All subgraphs are available via The Graph's hosted service:

**V1 Endpoints**
| Network | URL |
| --- | --- |
| Matic | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-matic |
| Gnosis | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-xdai |
| Optimism Mainnet | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-optimism-mainnet |
| Arbitrum One | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-arbitrum-one |
| Avalanche C-Chain | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-avalanche-c |
| BNB Chain | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-bsc-mainnet |
| Goerli | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-goerli |
| Mumbai | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-mumbai |
| Optimism Goerli | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-optimism-goerli |
| Arbitrum Goerli | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-arbitrum-goerli |
| Avalanche Fuji | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-v1-avalanche-fuji |

**Development Endpoints**
| Network | URL |
| --- | --- |
| Matic | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-matic |
| Gnosis | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-xdai |
| Optimism Mainnet | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-optimism-mainnet |
| Arbitrum One | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-arbitrum-one |
| Avalanche C-Chain | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-avalanche-c |
| BNB Chain | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-bsc-mainnet |
| Mumbai | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-mumbai |
| Goerli| https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-goerli |
| Optimism Goerli | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-optimism-goerli |
| Arbitrum Goerli | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-arbitrum-goerli |
| Avalanche Fuji | https://thegraph.com/explorer/subgraph/superfluid-finance/protocol-dev-avalanche-fuji |

*Note: Development endpoints will include features that are still in progress. Documentation will not reflect new features yet to be released in V1


# 🤓 Local development

🛑 **STOP - Do not pass Go** 🛑 You probably don't need this repo. If you need data for Superfluid on xDAI, Polygon (Matic), or testnet, we already deployed them for you! Head over to the **Docs** to get started.

In this section we will cover the following:

0. Deploy the subgraph to a `docker container`.
1. Deploy the Superfluid contracts to `Ganache`.
2. Check that your setup is correct by running some `tests`.

If you get stuck, see The Graph [docs](https://thegraph.com/docs/quick-start#local-development).

## Initial setup

First install these dependencies:

-   [docker](https://docs.docker.com/install/)
-   [docker-compose](https://docs.docker.com/compose/install/)

Now install the necessary node packages with the following commands:

```bash
npm i -g truffle ganache-cli @graphprotocol/graph-cli
yarn install
```

### Ganache

Start ganache. It's helpful to specify a mnemonic, so you can hard-code the address in `subgraph.yaml` and the test files.
We utilize the mnemonic from hardhat as all the addresses for the tests are based off this.
Open one terminal window and run this command, this window will now be running this in the background.
```bash
ganache-cli -h 0.0.0.0 -m "test test test test test test test test test test test junk"
```

### Graph-node

Download the `graph-node` Docker instance.

```bash
git clone https://github.com/graphprotocol/graph-node/
cd graph-node/docker
```

If on Linux, run the following script. You should be already logged into docker

```bash
# For Linux machines
sudo apt install jq
./setup.sh # writes the host IP to the docker-compose file
```

> Note: If you get a "version" error, update your docker-compose with [these instructions](https://docs.docker.com/compose/install/). If you get an error like `ERROR: could not find an available, non-overlapping IPv4 address...` then try turning off OpenVPN, or follow [this tutorial](https://stackoverflow.com/questions/45692255/how-make-openvpn-work-with-docker).

If you are on a mac, create a `setup_graph.sh` file in `graph-node/docker` and paste the following in it:
```
#!/bin/bash

docker-compose down -v;

if [ -d "data" ]
then
  echo "Found old data for the graph node - deleting it";
  # we need to sudo this to remove system locked files
  rm -rf data/;
fi

docker-compose up;
```

Also change line 20 in the `docker-compose.yml` to `ethereum: 'mainnet:http://host.docker.internal:8545'`.

Then run `chmod +x setup_graph.sh`, this makes the shell script executable.
Open another terminal window and run `./setup_graph.sh` and your local graph will start booting up.

> Note: You can check out this [blog post](https://medium.com/blockrocket/dapp-development-with-a-local-subgraph-ganache-setup-566a4d4cbb) for more details if you run into any issues.

> Another note: If you are using an M1 mac, follow the instructions [here](https://github.com/graphprotocol/graph-node/tree/master/docker#running-graph-node-on-an-macbook-m1) otherwise you will probably run into issues.

You should see ganache logs start coming in:

```
Listening on 0.0.0.0:8545
web3_clientVersion
net_version
eth_getBlockByNumber
eth_getBlockByNumber
```

If there is some connection issue between graph-node and ganache, it may be caused by docker network issues.

I had to run the following command to get the correct host IP, instead of the one that `setup.sh` provided.

```bash
ip a | grep docker | grep inet
> inet 172.17.0.1/16 brd 172.17.255.255 scope global docker0
```

Try changing the line in the docker-compose using the output above

```
ethereum: 'mainnet:http://172.17.0.1:8545'
```

## Deploy the contracts to Ganache

> Note: If you're returning from an earlier work session, skip down to [Testing](#testing).

Open your third terminal window and navigate to the **root of the repo** and run the build command - this compiles the ethereum contracts and creates the artifacts required in the next steps.

```bash
yarn build
```

This is also a good time to generate the typechain folder used throughout the tests, go into the `packages/ethereum-contracts` directory and call:
```bash
yarn install
yarn run generate-ethers-types
mv typechain ../subgraph
```

Now come back here in `packages/subgraph` and run the following command to deploy contracts:

```bash
npx hardhat run scripts/deployContractsAndToken.ts --network localhost
```

> Tip: You must use the mnemonic provided otherwise you will have to update all the test addresses in the relevant parts of the code.

## Deploy the Subgraph

Once the contracts and token have been deployed, you can run the following one liner or go step by step and run through the commands below.

```bash
# To build and deploy the Subgraph in a single line:
yarn prepare-local && yarn set-network-local && yarn getAbi && yarn generate-sf-meta-local && yarn codegen && yarn create-local && yarn deploy-local

# Step by step
# Generate `subgraph.yaml` using the test-subgraph.template.yaml
yarn prepare-local

# Generate `addresses.ts` using the addresses.template.ts file
yarn set-network-local

# Get the ABIs
yarn getAbi

# Generate the SFMeta file (this is used internally for to generate an internally used entity in prod, but still necessary for building/deploying the subgraph)
yarn generate-sf-meta-local

# Generate the subgraph schema
yarn codegen

# Check the subgraph will compile
yarn build

# Create the namespace for the subgraph (only run once)
yarn create-local

# Deploy the subgraph
yarn deploy-local

> Deployed to http://localhost:8000/subgraphs/name/superfluid-test/graphql
```

> If you see "NetworkNotSupported" error, make sure the network name in your docker-compose matches the one in subgraph.yaml.
> Note: The network must be mainnet when testing locally.

Navigate to the url in the console output, and let's try a test query so we know the deployment worked. It should return an empty `flowUpdatedEvents` array.

```
{
  flowUpdatedEvents {
    id
  }
}
```

:tada: Congrats! In the next section we will show you how to run tests and redeploy the subgraph with a clean slate.

## Running Tests

If you are continuing from the previous steps, you can immediately run the tests with the following command:

```bash
npx hardhat test --network localhost
```

> Note: If you get an error complaining about workspaces requiring an array, delete the workspaces property in the subgraph folder's package.json.

This goes over an integration tests which test that the data we are mapping to the subgraph is expected given the inputs for creating a flow, index, subscription, etc. If you're interested in learning about how the test code is structured, you can click [here](#test-structure) to learn more.

You can also run a data integrity test which checks the data mapped to the subgraph with data from the contracts:

```bash
npx hardhat run scripts/dataIntegrityTest.ts --network <NETWORK>
```

> Note: you must specify the network in the `hardhat.config.ts` file, you can look at how it is being done for matic and do so for any network you are interested in running this on. You can use a URL from Infura, Alchemy or from your own node.

## Re-deployments

To re-deploy the subgraph and contracts (for a clean slate), you need to kill the ganache and graph node processes (`ctrl + c` usually does it) and restart these and run through the previous steps again.

## Troubleshooting

### Docker

Sometimes docker container won't stop

`Cannot kill container d28... signaling init process caused "permission denied"`

Try running

```
sudo killall containerd-shim
```

### Contracts

Poke around at the contracts using truffle console. See our docs [Using Superfluid with truffle console](https://docs.superfluid.finance/superfluid/docs/setup-truffle-console) for more help.

```bash
truffle console --network ganache

const SuperfluidSDK = require("@superfluid-finance/js-sdk");
# For ganache
sf = new SuperfluidSDK.Framework({web3, version: "test"});
# For xDAI / testnets / etc
sf = new SuperfluidSDK.Framework({web3, resolver: "0xD2009765189164b495c110D61e4D301729079911"});

await sf.initialize();

const [admin, bob, carol, dan] = accounts;
```

### Subgraph

Another way to perform a sanity check is using a http query with the following:

| Property     | value                                                  |
| ------------ | ------------------------------------------------------ |
| URL          | `http://localhost:8000/subgraphs/name/superfluid-test` |
| Request type | POST                                                   |
| body         | GraphQL                                                |

```graphql
query {
    accounts(
        first: 1000
        where: { id_not: "0x0000000000000000000000000000000000000000" }
    ) {
        id
    }
}
```

You should get a response like this

```js
{
    "data": {
        "accounts": [
            {
                "balance": "0",
                "hat": null,
                "id": "0x0000000000000000000000000000000000000000"
            },
            {
                "balance": "90.001030003030003391",
                "hat": null,
                "id": "0xbf44e907c4b6583d2cd3d0a0c403403bb44c4a3c"
            }
        ]
    }
}
```

### Schema Overview
For the V1 Subgraph design, there are three "levels" of entities: event, higher order level (HOL) and aggregate. A brief explanation of each below:

## Event Entities
These entities are for the most part a 1-to-1 mapping with the raw events from the contract, however for some of them, we have added data, this is noted clearly in the `schema.graphql` file and will be viewable in the playground as well. These entities are created once and never updated.

## Higher Order Level Entities
The higher order level entities are an abstraction of the events is not ephemeral in the same way the event entities are. They contain data on an entity over its "lifetime" and therefore may be updated over time.

## Aggregate Entities
Aggregate entities are exactly what the name suggests - they are entities that store aggregate data. More specifically, we do this at an account-token level and also at a global token level.

### Test Structure

This section is intended for those who are interested in understanding the structure of the test suite.
The idea of this test suite is to ensure that we can be as confident as possible that the data is what we expect it to be based on our actions. We want to validate the data between each action. To accomplish this, we needed to store a global state for our HOL and aggregate entities and so it is important to follow the pattern of modify then update global state otherwise the tests break.

The entry point of the tests is the `subgraph.test.ts` file, this is where you specify the parameters and what you want to test. For example, you want to test indexing a newly created flow. There are helper functions for all the different things you may want to do and examples of everything in the test, but you must follow the pattern of modify then update global state as stated above.

The helper functions contain the following:
- action function: creates a txn and modifies the blockchain state
- initialize functions: initializes the data or gets the last saved data to be validated
- updater functions: these functions get what we expect the data to be, this will be compared to the data returned from the subgraph
- event validator function: this validates the newly created event
- hol/aggregate validator functions: these functions validate the hol or aggregate entities
- returns the updated data from the updater functions

Then we are back in `subgraph.test.ts` and the global state is updated with this new data.

# Production

## Graph Network

Coming soon. Let The Graph team know you want Superfluid available on the Network >> https://discord.gg/7eXTnPKYV4

## Hosted Service

Log in to the explorer at https://thegraph.com/explorer/. Then go to your dashboard. IMPORTANT: select the Superfluid-Finance account- don't use your your personal account. Copy the auth token to use in this command:

```
graph auth https://api.thegraph.com/deploy/ <ACCESS_TOKEN>
```

If the subgraph has not been created yet, you must create it first using the dashboard.

## Release Process

- Bump minor or patch version for the subgraph package: `yarn manage-versions && git commit packages/subgraph/package.json`.
- Select the appropriate Subgraph LTS branch: `release-subgraph-v1`.
- Merge `origin/dev` to the LTS branch.
- Create a new release in github.

## Developer Notes

When adding new networks, we must do the following:
- NOTE: The `network` field in the `/config/*.json` file must match the official naming of the network name from the [subgraph docs](https://thegraph.com/docs/developer/create-subgraph-hosted#supported-networks). This name can deviate from the name of the file itself. e.g. `avalanche-c.json` must have field: `"network": "avalanche"`.
- Add a new file to `./config`, the name of the file should be derived from the canonical network name in `packages/ethereum-contracts/truffle-config.js`. e.g. adding network `avalanche-c` to the array in `./networks.json` and then adding `avalanche-c.json` to `/config`. The name of the network specific file in `/config` will dictate our subgraph endpoint, e.g. `protocol-dev-avalanche-c`.
- We also need to add the host and resolver addresses to the `addresses.template.ts` file. NOTE: the `network` we are comparing in the `addresses.template.test` file should match the `"network"` field in `/config/*.json`.

# Contributing

Contributions, suggestions, and issues are welcome. At the moment, there are no strict guidelines to follow.
