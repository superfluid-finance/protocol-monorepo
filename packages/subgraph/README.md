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

<>{`\_`}</>

> Official subgraph for the superfluid protocol

:warning: WARNING: This subgraph is under active development! Please use with caution until it is finished

## What you can do:

-   **Use the Official rDAI subgraph** provided by TheGraph - see [link](https://thegraph.com/explorer/subgraph/rtoken-project/rdai).

-   **Develop and improve the subgraph** - see [Local development](#local-development).

-   **Implement your own rToken subgraph** - see [Bring-your-own rToken](#bring-your-own-rtoken).

## Local development

> Hold on :exclamation: You probably don't need this section. If you are using rToken on `Mainnet` or `Ropsten` you should just use an existing subgraph from The Graph's website.

The rToken team uses this local subgraph deployment flow to enable rapid development and testing of the tools provided here. In this section we will do the following:

1. Deploy the subgraph to a `docker container`.
2. Deploy the rToken contracts to `Ganache`.
3. Check that your setup is correct by running some `tests`.

### Initial setup

#### Notes:

-   If you've already performed this step, you should skip down to the [Testing and restarting](#testing-and-restarting).
-   If you get stuck, see The Graph [docs](https://thegraph.com/docs/quick-start#local-development).

First install the dependencie: [docker](https://docs.docker.com/install/) and [docker-compose](https://docs.docker.com/compose/install/)

Install the necessary packages:

```bash
yarn global add truffle ganache-cli @graphprotocol/graph-cli
```

Start ganache. It's helpful to specify a mnemonic, so you can hard-code the address in `subgraph.yaml` and the test files.

```bash
ganache-cli -h 0.0.0.0 -m 'deputy taste judge cave mosquito supply hospital clarify argue aware abuse glory'
```

Download the `graph-node` Docker instance.

```bash
git clone https://github.com/graphprotocol/graph-node/
```

Then navigate to `graph-node/docker`

If on Linux, run the following script.

> Note I had problems here. If you get a "version" error, update your docker-compose with [these instructions](https://docs.docker.com/compose/install/). If you get an error like `ERROR: could not find an available, non-overlapping IPv4 address...` then try turning off OpenVPN, or follow [this tutorial](https://stackoverflow.com/questions/45692255/how-make-openvpn-work-with-docker).

```bash
# For Linux machines
sudo apt install jq
./setup.sh # writes the host IP to the docker-compose file
```

Now start the necessary subgraph Docker containers.

```bash
docker-compose up
# or to run in background
docker-compose up -d

# Check its running
docker logs docker_graph-node_1
```

#### Deploy the contracts to Ganache

Now in `rtoken-monorepo` navigate to `packages/contracts`, deploy the contracts, and copy the address for `rTOKEN contract (proxy)`.

```bash
# Install dependencies
lerna bootstrap
# Deploy contracts
truffle test --network subgraph test/subgraphDeployment.test.js
# Copy the rToken contract address
> ...
> The rTOKEN contract (proxy) is deployed at: 0x625aD1AA6469568ded0cd0254793Efd0e5C0394F
```

We also need to build the contracts, for use in the `subgraph` package.

```bash
# Build and save the contracts
yarn build
```

#### Deploy the Subgraph

Now in `rtoken-monorepo` navigate to `packages/subgraph` and paste the contract address in `subgraph.yaml`. We are ready to deploy our subgraph.

```bash
# Generate the subgraph.yaml file from the template
yarn prepare:local

# Copy the abis from packages/contracts and generate the subgraph schema
yarn codegen

# Create the subgraph node "rtoken-test" (only run once)
yarn create-local

# Deploy the subgraph to "rtoken-test" node
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

## Bring-your-own rToken

If you deploy your own token, and wish to use this subgraph with `@rtoken/utils` to get data, you will need to deploy you own subgraph. As long as you didn't modify the rToken contracts too much, you can probably deploy as-is. Be sure to modify `subgraph.yaml` with the correct `address` and `startBlock`.

# Contributing

Contributions, suggestions, and issues are welcome. At the moment, there are no strict guidelines to follow.
