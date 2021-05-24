## Todo

- Set up deployment for testnet/mainnet using approach of [sablier-subgraph](https://github.com/sablierhq/sablier-subgraph/blob/master/subgraph.template.yaml). Allows keep multiple subgraphs on the same git branch, so you can run deployments like this: `yarn run deploy:mainnet` or `yarn run deploy:kovan`, rather than swap the contract addresses and network name just for deployment purposes and then revert
- Add deployment Github Action [example](https://github.com/sablierhq/sablier-subgraph/blob/master/.github/workflows/deploy.yml)
