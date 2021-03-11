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

## What you can do:

-   **Use the Official rDAI subgraph** provided by TheGraph - see [link coming soon]().

-   **Help improve the subgraph** - see [Local development](#local-development).

-   **Implement your own Superfluid subgraph (please chat with us first)** - see [Deploy Your Own](#deploy-your-own).

## Local development

> Hold up :exclamation: You probably don't need this repo. If you are using Superfluid on xDAI, Polygon (Matic), or another testnet, you should just use the existing subgraph from The Graph Network.

In this section we will cover the following:

1. Deploy the subgraph to a `docker container`.
2. Deploy the Superfluid contracts to `Ganache`.
3. Check that your setup is correct by running some `tests`.

If you get stuck, see The Graph [docs](https://thegraph.com/docs/quick-start#local-development).

### Initial setup

First install these dependencies:

-   [docker](https://docs.docker.com/install/)
-   [docker-compose](https://docs.docker.com/compose/install/)

Now install the necessary node packages:

```bash
yarn global add truffle ganache-cli @graphprotocol/graph-cli
```

#### Ganache

Start ganache. It's helpful to specify a mnemonic, so you can hard-code the address in `subgraph.yaml` and the test files.

```bash
ganache-cli -h 0.0.0.0 -m 'deputy taste judge cave mosquito supply hospital clarify argue aware abuse glory'
```

#### Graph-node

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
# or to run in background
docker-compose up -d

# Check its running
docker logs docker_graph-node_1
```

#### Deploy the contracts to Ganache

> If you're returning from an earlier coding session, you should skip down to the [Testing and restarting](#testing-and-restarting).

Now in navigate to `protocol-monorepo/packages/ethereum-contracts`. To deploy the contracts run the following commands

```bash
yarn build

DISABLE_NATIVE_TRUFFLE=true truffle --network ganache exec "./scripts/deploy-test-environment.js"
```

Finally, copy the address for the `Resolver`.

> Tip: If you use the same mnemonic each time you start Ganache and deploy the contracts, then this will always be the same address.

host: 0x72D802E9afAb5852B4309236550B0Dac5ebCd301
cfa: 0x5BB3095d09a7Bc7cE8C129e9A8CBFa21e9b36416
ida: 0xdb594725203c59F20F9b5fB2D360947523dabd27
resolver: 0xa36FfB4643C11307515F9851f2320a0556fD2687

#### Deploy the Subgraph

Here in `packages/subgraph` enter the contract addresses from the previous step in `config/local/json`.

We are now ready to deploy our subgraph.

```bash
# Generate "subgraph.yaml" using the template
yarn prepare-local

# Get the ABIs
yarn getAbi

# Generate the subgraph schema
yarn codegen

# Create the namespace for the subgraph (only run once)
yarn create-local

# Deploy the subgraph
yarn deploy-local
```

Great job! Now let's make sure things are working properly by doing a sanity check using Postman, or another API tool.

| Property     | value                                              |
| ------------ | -------------------------------------------------- |
| URL          | `http://localhost:8000/subgraphs/name/rtoken-test` |
| Request type | POST                                               |
| body         | GraphQL                                            |

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

:tada: Congrats! if you were successful with the initial setup, you can move to the next section to enable automatic redeployments of the subgraph upon changes.

### Testing and restarting

Once you've completed the initial setup, here is the flow for testing and restarting your subgraph.

1. In the repo `graph-node/docker`, stop your docker instance, and restart it:

```bash
sudo rm -rf data && docker-compose up

```

2. Deploy the contracts to ganache (if needed)

3. Re-deploy the new subgraph, whenever subgraph.yaml is changed:

```bash
yarn create-local
yarn deploy-local --watch
```

## Deploy Your Own

If you deploy your own token, and wish to use this subgraph with `@rtoken/utils` to get data, you will need to deploy you own subgraph. As long as you didn't modify the rToken contracts too much, you can probably deploy as-is. Be sure to modify `subgraph.yaml` with the correct `address` and `startBlock`.

# Contributing

Contributions, suggestions, and issues are welcome. At the moment, there are no strict guidelines to follow.
