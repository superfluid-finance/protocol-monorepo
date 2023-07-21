# Superfluid Metadata

Contains metadata around the Superfluid framework.  
The goal of this repository is to make it as easy as possible to reference contained metadata from various contexts (e.g. backend script or browser page) and independently of the tech stack used there. Convenience wrappers are provided for JS/TS, other environments can fall back to parsing plain JSON files.

## Making Changes

When adding new changes (new addresses), add it to `networks.json` and make sure to run `./build.sh` before pushing so that both the list files in `main` and `module` are updated.
If you are adding a new property, please also modify the .d.ts files in the module folder accordingly as well.

## Networks

List of EVM networks with the Superfluid protocol deployed.
Example uses

### Use in an HTML page

This example uses the jsDelivr CDN in order to always reference the latest version of the networks list.  
You can of course also self-host a copy or use another service, anything signalling the right mime type and with a compatible CORS policy should work.

```html
<script type="module">
  import networks from "https://cdn.jsdelivr.net/gh/superfluid-finance/metadata/module/networks/index.js";
  // example use
  const network = networks.getNetworkByChainId(networkId);
```

### Use in a nodejs project using ES modules

Nodejs has stable support for ES modules since version 14.  
In order to switch a project to using ES modules by default, the package.json needs to contain a field
```json
"type": "module"
```
(for other ways to enable ES modules, see [the nodejs docs](https://nodejs.org/api/esm.html#enabling))

If that's the case, you can use the networks metadata like this:
```js
import sfMeta from "@superfluid-finance/metadata";

// example use
const network = sfMeta.getNetworkByName("eth-goerli");
```

### Use in a nodejs project using CommonJS (legacy)

Nodejs projects still using CommonJS (if you're unsure, check [the docs](https://nodejs.org/api/packages.html#determining-module-system)), can use ES modules using the syntax of [dynamic imports](https://nodejs.org/api/esm.html#import-expressions):
```js
import("@superfluid-finance/metadata").then(module => {
  const sfMeta = module;

  // example use
  const network = sfMeta.getNetworkByName("eth-goerli");
}
```

Alternative using await:
```js
const sfMetaPromise = import("@superfluid-finance/metadata");
(async () => {
  const sfMeta = (await sfMetaPromise).default;

  // example use
  const network = sfMeta.getNetworkByName("eth-goerli");
}
```

### Use in a node REPL:

The repl uses CommonJS too, thus usage looks like this:

```js
let sfMeta
import("@superfluid-finance/metadata").then(module => sfMeta = module)
> sfMeta.networks.length
12
> sfMeta.testnets.length
7
> sfMeta.mainnets.length
5
> sfMeta.mainnets.filter(n => n.nativeTokenSymbol === "ETH").map(n => n.name)
[
  'eth-goerli',
  'optimism-goerli',
  'arbitrum-goerli',
  'optimism-mainnet',
  'arbitrum-one'
]
> m.getNetworkByChainId(10)
{
  name: 'optimism-mainnet',
  isTestnet: false,
  networkId: 10,
  chainId: 10,
  shortName: 'optimism',
  nativeTokenSymbol: 'ETH',
  contractsV1: {
    resolver: '0x743B5f46BC86caF41bE4956d9275721E0531B186',
    host: '0x567c4B141ED61923967cA25Ef4906C8781069a10',
    governance: '0x0170FFCC75d178d426EBad5b1a31451d00Ddbd0D',
    cfaV1: '0x204C6f131bb7F258b2Ea1593f5309911d8E458eD',
    idaV1: '0xc4ce5118C3B20950ee288f086cb7FC166d222D4c',
    superTokenFactory: '0x8276469A443D5C6B7146BED45e2abCaD3B6adad9',
    superfluidLoader: '0x8E310ce29Ab7Fa2878944A65BB0eaF97B1853d40',
    toga: '0xA3c8502187fD7a7118eAD59dc811281448946C8f'
  },
  startBlockV1: 4300000,
  logsQueryRange: 50000,
  explorer: 'https://optimistic.etherscan.io',
  subgraphV1: {
    name: 'protocol-v1-optimism-mainnet',
    hostedEndpoint: 'https://api.thegraph.com/subgraphs/name/superfluid-finance/protocol-v1-optimism-mainnet'
  }
}
```

### Use in a bash script

With the help of [jq](https://jqlang.github.io/jq/), it's also possible to parse metadata from within a bash script.
Here's an example:

```sh
#!/bin/bash

# exit on error or undefined var
set -eu

metadata=$(curl -f -s "https://raw.githubusercontent.com/superfluid-finance/metadata/master/networks.json")
testnets=$(echo "$metadata" | jq -r '.[] | select(.isTestnet == false).name')

for network in $testnets; do
        host=$(echo "$metadata" | jq -r '.[] | select(.name == "'$network'").contractsV1.host')
        native_token_wrapper=$(echo "$metadata" | jq -r '.[] | select(.name == "'$network'").nativeTokenWrapper')

        echo "$network | host address: $host, native token wrapper address: $native_token_wrapper"
done
```
