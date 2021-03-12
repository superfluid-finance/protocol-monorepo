<h1 align="center">Welcome to Superfluid Subgraph 👋
</h1>
<div align="center">
<img  width="300" padding="0 0 10px" alt="Superfluid logo" src="/sf-logo.png" />
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

<>{`\_}</>

> Official subgraph for the superfluid protocol

🚧⚠️🚧⚠️

**WARNING: This subgraph is under active development!**

Please use with caution until it is finished

🚧⚠️🚧⚠️

# Hosted Subgraphs

The Superfluid subgraphs are available from The Graph's hosted service, at the following endpoints:

TODO

# Local development

🛑 **STOP** 🛑 You probably don't need this repo. If you are using Superfluid on xDAI, Polygon (Matic), or even a testnet, we've already deployed the subgraphs for you. See **Hosted Subraphs** above.

In this section we will cover the following:

1. Deploy the subgraph to a `docker container`.
2. Deploy the Superfluid contracts to `Ganache`.
3. Check that your setup is correct by running some `tests`.

If you get stuck, see The Graph [docs](https://thegraph.com/docs/quick-start#local-development).

## Initial setup

First install these dependencies:

-   [docker](https://docs.docker.com/install/)
-   [docker-compose](https://docs.docker.com/compose/install/)

Now install the necessary node packages:

```bash
yarn global add truffle ganache-cli @graphprotocol/graph-cli
```

### Ganache

Start ganache. It's helpful to specify a mnemonic, so you can hard-code the address in `subgraph.yaml` and the test files.

```bash
ganache-cli -h 0.0.0.0 -m 'deputy taste judge cave mosquito supply hospital clarify argue aware abuse glory'
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

Now start the necessary subgraph Docker containers.

```bash
docker-compose up
```

You should see ganache logs start coming in

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

Ty changing the line in the docker-compose using the output above

```
ethereum: 'ganache:http://172.17.0.1:8545'
```

## Deploy the contracts to Ganache

> Note: If you're returning from an earlier work session, skip down to [Testing](#testing).

Now navigate to `packages/ethereum-contracts` and build the contracts.

```bash
yarn build
```

Now come back here in `packages/subgraph` and run the following command to deploy contracts:

```bash
NEW_TEST_RESOLVER=1 DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "../ethereum-contracts/scripts/deploy-test-environment.js"

> ConstantFlowAgreementV1: TruffleContract .agreements.cfa @0x5BB3095d09a7Bc7cE8C129e9A8CBFa21e9b36416 | Helper .cfa

> =============== TEST ENVIRONMENT RESOLVER ======================
> export TEST_RESOLVER_ADDRESS=0xa36FfB4643C11307515F9851f2320a0556fD2687
```

Copy the `export` command from your output and run it. Also copy the address for `ConstantFlowAgreementV1`.

> Tip: If you use the same mnemonic each time you start Ganache and deploy the contracts, then this will always be the same address.

## Deploy the Subgraph

Enter the address for `ConstantFlowAgreementV1` from the previous step in `config/ganache.json`.

We are now ready to deploy our subgraph.

```bash
# Generate "subgraph.yaml" using the template
yarn prepare-local

# Get the ABIs
yarn getAbi

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

> If you see "NetworkNotSupported" error, make sure the network name in your docker-compose matches the one in subgraph.yaml

Navigate to the url in the console output, and let's try a test query so we know the deployment worked. It should return an empty `accounts` array

```
{
  accounts{
    flowsOwned{
      id
    }
  }
}
```

:tada: Congrats! In the next section we will setup automatic subgraph re-deployments and run tests.

## Testing

Now that you have everything set up, you can start making changes to the subgraph. To automatically re-reploy the subgraph, we will use the `--watch` flag.

You will use this flow anytime you want to restart, or come back to the subgraph.

1. In the repo `graph-node/docker`, stop your docker instance, and restart it:

```bash
# Blow away the database
sudo rm -rf data
docker-compose up
```

2. Deploy the contracts to ganache (if needed)

Remember to use the same mnemonic, or update the contract address in `subgraph.yaml`

3. Start subgraph auto re-deploy

```bash
yarn create-local
yarn deploy-local --watch
```

## Troubleshooting

### Contracts

Poke around at the contracts using truffle console. See our docs [Using Superfluid with truffle console](https://docs.superfluid.finance/superfluid/docs/setup-truffle-console) for more help.

```bash
truffle console --network ganache

const SuperfluidSDK = require("@superfluid-finance/js-sdk");
sf = new SuperfluidSDK.Framework({web3,tokens: ["fDAI"], resolverAddress: "0xa36FfB4643C11307515F9851f2320a0556fD2687"});

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
        balance
        hat {
            id
        }
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

# Production

## Graph Network

TODO

## Hosted Service

TODO

# Contributing

Contributions, suggestions, and issues are welcome. At the moment, there are no strict guidelines to follow.
