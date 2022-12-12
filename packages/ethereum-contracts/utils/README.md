## SuperToken Deployer

This is a single-file Dapp for deploying SuperToken wrappers of existing ERC20 tokens.
Requires a browser with injected web3 provider (e.g. Metamask) and a local webserver.
If you have python installed, you can start a webserver with the document root set to the current directory with
```python
python -m SimpleHTTPServer 1337
```
Then navigate to http://localhost:1337/supertoken-deployer.html

Alternatively, you can also find it on IPFS: [QmbjeSLXfmePAnWRBhkhajCpdu58Xsn7BqQwj8pSxRS9d8](https://ipfs.io/ipfs/QmaCBCARcUthLcG1sNYC8SAE7okuDUZqbythvWuYozBpgC)

## Stream Closer

This is a single-file Dapp for closing streams / deleting flows.
The flow parameters _token_, _sender_ and _receiver_ can be set either in the UI or provided as URL parameters.
If you have python installed, you can start a webserver with the document root set to the current directory with
```python
python -m SimpleHTTPServer 1337
```

Then navigate to http://localhost:1337/stream-closer.html

Alternatively, you can also find it on IPFS: [QmPu6h7w2LCAeLdN4GVR2CTa7wTtEsi4iJokXTW7QiQy2z](https://ipfs.io/ipfs/QmPu6h7w2LCAeLdN4GVR2CTa7wTtEsi4iJokXTW7QiQy2z)

In order to set the parameter in the URL, use this format:
stream-closer.html?chainId=<chainId>&token=<tokenAddress>&sender=<senderAddress>&receiver=<receiverAddress>

## MFA Tester

This is a single-file dApp for testing the MultiFlowTesterApp contract.

There are a few steps to complete to get everything up and running:
1. Open a terminal window and start a hardhat node instance: `npx hardhat node` in `packages/ethereum-contracts`
2. Open another terminal window and deploy contracts and token: `yarn deploy-contracts-local` in `packages/subgraph`
3. Deploy MFA contract: `npx hardhat run ops-scripts/deploy-mfa.ts` in `packages/ethereum-contracts`
4. Build SDK-Core: `yarn build` in `packages/sdk-core` and copy `packages/sdk-core/dist/index.umd.js` into `packages/ethereum-contracts/utils/dist`.

> NOTE: There are hardcoded addresses for the resolver and MFA in mfa-tester.html, make sure that these addresses are in sync with what is deployed otherwise it won't work.

If you have python installed, you can start a webserver with the document root set to the current directory with
```python
python -m SimpleHTTPServer 1337
```
Then navigate to http://localhost:1337/mfa-tester.html
